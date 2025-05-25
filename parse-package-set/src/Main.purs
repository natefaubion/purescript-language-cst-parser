module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (parseJson, decodeJson, printJsonDecodeError)
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
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, runAff_, throwError, error)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throw, throwException)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.ChildProcess (SpawnSyncOptions)
import Node.ChildProcess as Exec
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Glob.Basic as Glob
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (stderr)
import Node.Process as Process
import Node.Stream as Stream
import PureScript.CST (RecoveredParserResult(..), parseModule, printModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.ModuleGraph (sortModules, ModuleSort(..))
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types (Module(..), ModuleHeader)

foreign import tmpdir :: String -> Effect String

foreign import hrtime :: Effect { seconds :: Number, nanos :: Number }

foreign import hrtimeDiff :: { seconds :: Number, nanos :: Number } -> Effect { seconds :: Number, nanos :: Number }

execSpawn :: String -> Array String -> (SpawnSyncOptions -> SpawnSyncOptions) -> Effect String
execSpawn proc args options = do
  res <- Exec.spawnSync' proc args options
  case res.exitStatus of
    Normally 0 ->
      Buffer.toString UTF8 res.stdout
    _ -> do
      _ <- Stream.write stderr res.stderr
      throw $ "Child process failed: " <> proc <> " " <> Str.joinWith " " args

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  dir <- liftEffect $ Process.cwd
  let installPath = Path.concat [ dir, "parse-package-set", "package-set-install" ]
  s <- liftEffect $ execSpawn "spago" [ "ls", "packages", "--json" ] (_ { cwd = Just installPath })
  packages <- case decodeJson =<< parseJson s of
    Left err -> throwError $ error $ printJsonDecodeError err
    Right (object :: Object Json) -> pure $ Object.keys object
  _ <- liftEffect $ execSpawn "spago" ([ "install" ] <> packages) (_ { cwd = Just installPath })

  pursFiles <- Array.fromFoldable <$> Glob.expandGlobs (Path.concat [ installPath, ".spago", "p" ]) [ "*/src/**/*.purs" ]
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
