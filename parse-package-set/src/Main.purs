module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Filterable (partitionMap)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un)
import Data.Number.Format as NF
import Data.String as Str
import Data.String.CodeUnits as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, runAff_, throwError, error)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throwException)
import Node.Buffer as Buffer
import Node.ChildProcess as Exec
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, stat, writeTextFile)
import Node.FS.Stats as FS
import Node.Path (FilePath)
import PureScript.CST (RecoveredParserResult(..), parseModule, printModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.ModuleGraph (sortModules, ModuleSort(..))
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types (Module(..), ModuleHeader)
import PureScript.CST.ModuleGraph (sortModules, ModuleSort(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (parseJson, decodeJson, printJsonDecodeError)
import Foreign.Object (Object)
import Foreign.Object as Object

foreign import tmpdir :: String -> Effect String

foreign import hrtime :: Effect { seconds :: Number, nanos :: Number }

foreign import hrtimeDiff :: { seconds :: Number, nanos :: Number } -> Effect { seconds :: Number, nanos :: Number }

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  tmpPath <- liftEffect $ tmpdir "cst-integration-"

  liftEffect $ Console.log $ "Making new project in " <> tmpPath

  writeTextFile UTF8 (tmpPath <> "/spago.yaml") defaultSpagoYaml

  s <- liftEffect $ Buffer.toString UTF8 =<< Exec.execSync' "spago ls packages --json" (_ { cwd = Just tmpPath })
  packages <- case decodeJson =<< parseJson s of
    Left err -> throwError $ error $ printJsonDecodeError err
    Right (object :: Object Json) -> pure $ Object.keys object
  _ <- liftEffect $ Exec.execSync' ("spago install " <> Str.joinWith " " packages) (_ { cwd = Just tmpPath })

  pursFiles <- getPursFiles 0 (tmpPath <> "/.spago")
  moduleResults <- parseModulesFromFiles pursFiles

  let
    partition = moduleResults # partitionMap \{ path, errors, duration, printerMatches } ->
      if Array.null errors then
        Right { path, duration, printerMatches }
      else
        Left { path, errors, duration }

  liftEffect $ forWithIndex_ partition.left \ix failed -> do
    let
      message = Array.intercalate "\n"
        [ "---- [Error " <> show (ix + 1) <> " of " <> show (Array.length partition.left) <> ". Failed in " <> formatMs failed.duration <> " ] ----"
        , ""
        , Array.intercalate "\n" $ foldMap formatError failed.errors
        ]

      formatError error =
        [ "  " <> failed.path <> ":" <> show (error.position.line + 1) <> ":" <> show (error.position.column + 1)
        , "  " <> printParseError error.error <> " at line " <> show (error.position.line + 1) <> ", column " <> show (error.position.column + 1)
        , ""
        ]
    Console.error message

  let
    successMessage = Array.intercalate " "
      [ "Successfully parsed"
      , show (Array.length partition.right)
      , "of"
      , show (Array.length pursFiles)
      , "modules."
      ]

  liftEffect $ Console.log successMessage
  liftEffect $ Console.log $ displayDurationStats (getDurationStats partition.right) "Success Case"

  let
    printerSucceeded = Array.filter (_.printerMatches >>> eq (Just true)) partition.right

    printerSuccessMessage = Array.intercalate " "
      [ "Successfully printed"
      , show (Array.length printerSucceeded)
      , "of"
      , show (Array.length partition.right)
      , "successully parsed modules."
      ]

  liftEffect $ Console.log printerSuccessMessage

  let
    printerFailed = Array.filter (_.printerMatches >>> eq (Just false)) partition.right

    printerFailedMessage = Array.intercalate " "
      [ "Printer failed for"
      , show (Array.length printerFailed)
      , "of"
      , show (Array.length partition.right)
      , "successfully parsed modules."
      ]

  unless (Array.null printerFailed) $ liftEffect do
    Console.error printerFailedMessage
    forWithIndex_ printerFailed \ix failed -> do
      let
        message = Array.intercalate "\n"
          [ "---- [Printer Error " <> show (ix + 1) <> " of " <> show (Array.length printerFailed) <> "] ----"
          , ""
          , failed.path
          ]
      Console.error message

  let
    mods = Array.mapMaybe _.mbModule moduleResults

  liftEffect case sortModules identity mods of
    Sorted sorted -> Console.log $ Array.intercalate " "
      [ "Successfully sorted module graph for"
      , show (Array.length sorted)
      , "of"
      , show (Array.length partition.right)
      , " successfully parsed modules."
      ]
    CycleDetected _ -> Console.log $ Array.intercalate " "
      [ "Error: cycle detected in module graph"
      ]

defaultSpagoYaml :: String
defaultSpagoYaml = Array.intercalate "\n"
  [ "package:"
  , "  name: test-parser"
  , "  dependencies: []"
  , "workspace:"
  , "  package_set:"
  , "    registry: 64.9.0"
  , "  extra_packages: {}"
  ]

getPursFiles :: Int -> FilePath -> Aff (Array FilePath)
getPursFiles depth root = do
  readdir root >>= foldMap \file -> do
    let path = root <> "/" <> file
    stats <- stat path
    if FS.isDirectory stats then
      if depth == 2 && file /= "src" then do
        pure []
      else
        getPursFiles (depth + 1) path
    else if Regex.test pursRegex path then
      pure [ path ]
    else pure []
  where
  pursRegex = unsafeRegex "\\.purs$" noFlags

type ModuleResult =
  { path :: FilePath
  , errors :: Array PositionedError
  , duration :: Milliseconds
  , mbModule :: Maybe (ModuleHeader Void)
  , printerMatches :: Maybe Boolean
  }

parseModulesFromFiles :: Array FilePath -> Aff (Array ModuleResult)
parseModulesFromFiles pursFiles = do
  block <- AVar.empty

  for_ (Array.range 1 10) \_ -> do
    liftEffect $ EffectAVar.put unit block mempty

  flip parTraverse pursFiles \file -> do
    AVar.take block
    result <- parseModuleFromFile file
    _ <- liftEffect $ EffectAVar.put unit block mempty
    pure result

parseModuleFromFile :: FilePath -> Aff ModuleResult
parseModuleFromFile path = do
  contents <- readTextFile UTF8 path
  before <- liftEffect hrtime
  let parsed = parseModule contents
  duration <- liftEffect $ hrtimeDiff before
  let
    durationMillis = Milliseconds $ duration.seconds * 1000.0 + duration.nanos / 1000000.0

    errors = case parsed of
      ParseSucceeded _ -> []
      ParseSucceededWithErrors _ errs -> NEA.toArray errs
      ParseFailed err -> [ err ]

    mbModule = case parsed of
      ParseSucceeded (Module mod) -> Just mod.header
      ParseSucceededWithErrors _ _ -> Nothing
      ParseFailed _ -> Nothing

    printerMatches = case parsed of
      ParseSucceeded mod ->
        pure $ contents == printModule mod
      ParseSucceededWithErrors mod _ ->
        pure $ contents == printModule mod
      ParseFailed _ -> Nothing

  pure
    { path
    , errors
    , mbModule
    , duration: durationMillis
    , printerMatches
    }

type DurationStats r =
  { minDuration :: Array { path :: FilePath, duration :: Milliseconds | r }
  , maxDuration :: Array { path :: FilePath, duration :: Milliseconds | r }
  , mean :: Milliseconds
  , total :: Milliseconds
  }

getDurationStats :: forall r. Array { path :: FilePath, duration :: Milliseconds | r } -> DurationStats r
getDurationStats res =
  { minDuration: Array.take 20 sorted
  , maxDuration: Array.reverse (Array.takeEnd 20 sorted)
  , mean
  , total: Milliseconds sum.duration
  }
  where
  sorted =
    Array.sortBy (comparing _.duration) res

  sum =
    sorted
      # foldMap (\{ duration: Milliseconds duration } -> Additive { duration, total: 1.0 })
      # un Additive

  mean =
    sum
      # \{ duration, total } -> Milliseconds (duration / total)

displayDurationStats :: forall r. DurationStats r -> String -> String
displayDurationStats { minDuration, maxDuration, mean, total } title =
  Array.intercalate "\n"
    [ ""
    , "---- [ " <> title <> " Timing Information ] ----"
    , "Fastest Parse Times:"
    , Array.intercalate "\n" $ displayLine <$> minDuration
    , ""
    , "Slowest Parse Times:"
    , Array.intercalate "\n" $ displayLine <$> maxDuration
    , ""
    , "Total Parse: " <> formatMs total
    , "Mean Parse: " <> formatMs mean
    ]

  where
  displayLine { path, duration } =
    String.takeRight 12 ("        " <> formatMs duration) <> "  " <> path

formatMs :: Milliseconds -> String
formatMs (Milliseconds ms) = NF.toStringWith (NF.fixed 3) ms <> "ms"
