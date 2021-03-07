module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (foldMap)
import Data.Array as Array
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
import Data.Tuple (Tuple, snd)
import Effect (Effect)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, runAff_)
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
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser (Recovered)
import PureScript.CST.Parser as Parser
import PureScript.CST.Parser.Monad (PositionedError, runParser)
import PureScript.CST.TokenStream (TokenStream)
import PureScript.CST.Types (PSModule)

foreign import tmpdir :: String -> Effect String

foreign import hrtime :: Effect { seconds :: Number, nanos :: Number }

foreign import hrtimeDiff :: { seconds :: Number, nanos :: Number } -> Effect { seconds :: Number, nanos :: Number }

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  tmpPath <- liftEffect $ tmpdir "cst-integration-"

  writeTextFile UTF8 (tmpPath <> "/spago.dhall") defaultSpagoDhall

  let execOpts = Exec.defaultExecSyncOptions { cwd = Just tmpPath }
  s <- liftEffect $ Buffer.toString UTF8 =<< Exec.execSync "spago ls packages" execOpts
  let lines = Str.split (Str.Pattern "\n") s
  let packages = Str.joinWith " " (String.takeWhile (_ /= ' ') <$> lines)
  _ <- liftEffect $ Exec.execSync ("spago install " <> packages) execOpts

  pursFiles <- getPursFiles 0 (tmpPath <> "/.spago")

  block <- AVar.empty

  for_ (Array.range 1 10) \_ -> do
    liftEffect $ EffectAVar.put unit block mempty

  moduleResults <- flip parTraverse pursFiles \file -> do
    AVar.take block
    result <- parseModuleFromFile file
    _ <- liftEffect $ EffectAVar.put unit block mempty
    pure result

  let
    partition = moduleResults # partitionMap \{ path, errors, duration } ->
      if Array.null errors then
        Right { path, duration }
      else
        Left { path, errors, duration }

  liftEffect $ forWithIndex_ partition.left \ix failed -> do
    let
      message = Array.intercalate "\n"
        [ "---- [Error " <> show (ix + 1) <> " of " <> show (Array.length partition.left) <> ". Failed in "<> formatMs failed.duration <> " ] ----"
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

-- TODO: Upgrade packages ref to 0.14 package set
defaultSpagoDhall :: String
defaultSpagoDhall = Array.intercalate "\n"
  [ "{ name = \"test-parser\""
  , ", dependencies = [] : List Text"
  , ", packages = https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca"
  , ", sources = [] : List Text"
  , "}"
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
  }

parseModuleFromFile :: FilePath -> Aff ModuleResult
parseModuleFromFile path = do
  contents <- readTextFile UTF8 path
  before <- liftEffect hrtime
  let parsed = parse (lex contents)
  duration <- liftEffect $ hrtimeDiff before
  let durationMillis = Milliseconds $ duration.seconds * 1000.0 + duration.nanos / 1000000.0
  pure
    { path
    , errors: either pure snd parsed
    , duration: durationMillis
    }

parse :: TokenStream -> Either PositionedError (Tuple (Recovered PSModule) (Array PositionedError))
parse tokenStream =
  runParser tokenStream Parser.parseModule

type DurationStats r =
  { minDuration :: Array { path :: FilePath, duration :: Milliseconds | r }
  , maxDuration :: Array { path :: FilePath, duration :: Milliseconds | r }
  , mean :: Milliseconds
  }

getDurationStats :: forall r. Array { path :: FilePath, duration :: Milliseconds | r } -> DurationStats r
getDurationStats res =
  { minDuration: Array.take 20 sorted
  , maxDuration: Array.reverse (Array.takeEnd 20 sorted)
  , mean
  }
  where
  sorted =
    Array.sortBy (comparing _.duration) res

  mean =
    sorted
      # foldMap (\{ duration: Milliseconds duration } -> Additive { duration, total: 1.0 })
      # un Additive
      # \{ duration, total } -> Milliseconds (duration / total)

displayDurationStats :: forall r. DurationStats r -> String -> String
displayDurationStats { minDuration, maxDuration, mean } title =
  Array.intercalate "\n"
    [ ""
    , "---- [ " <> title <> " Timing Information ] ----"
    , "Fastest Parse Times:"
    , Array.intercalate "\n" $ displayLine <$> minDuration
    , ""
    , "Slowest Parse Times:"
    , Array.intercalate "\n" $ displayLine <$> maxDuration
    , ""
    , "Mean Parse: " <> formatMs mean
    ]

  where
  displayLine { path, duration } =
    String.takeRight 12 ("        " <> formatMs duration) <> "  " <> path

formatMs :: Milliseconds -> String
formatMs (Milliseconds ms) = NF.toStringWith (NF.fixed 3) ms <> "ms"
