module Main where

import Prelude

import Control.Monad.Free (runFree)
import Control.Parallel (parTraverse)
import Data.Array (foldMap)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Filterable (partitionMap)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (over2, un, unwrap)
import Data.NonEmpty (NonEmpty(..), foldl1)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AVar as EffectAVar
import Effect.Aff (Aff, runAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throwException)
import Effect.Now (now)
import Node.ChildProcess as Exec
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, stat, writeTextFile)
import Node.FS.Stats as FS
import Node.Path (FilePath)
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser as Parser
import PureScript.CST.TokenStream (TokenStream)
import PureScript.CST.Types (Module)
import Text.Parsing.Parser (ParseError)
import Text.Parsing.Parser as Parsing

foreign import tmpdir :: String -> Effect String

main :: Effect Unit
main = runAff_ (either throwException mempty) do
  tmpPath <- liftEffect $ tmpdir "cst-integration-"

  writeTextFile UTF8 (tmpPath <> "/spago.dhall") defaultSpagoDhall

  let installCmd = "cd " <> tmpPath <> "; spago -x spago.dhall ls packages | cut -f 1 -d \' \' | xargs spago install"

  _ <- liftEffect $ Exec.execSync installCmd Exec.defaultExecSyncOptions

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
    partition = moduleResults # partitionMap \{ path, parsed, duration } -> case parsed of
      Left parseError -> Left { path, parseError, duration }
      Right parsedModule -> Right { path, parsedModule, duration }

  liftEffect $ forWithIndex_ partition.left \ix failed -> do
    let
      message = Array.intercalate "\n"
        [ "---- [Error " <> show (ix + 1) <> " of " <> show (Array.length partition.left) <> ". Failed in "<> show failed.duration <> " ] ----"
        , "Failed to parse module at path:"
        , failed.path
        , ""
        , "With error:"
        , show failed.parseError
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

  let
    mbFailureStats = getDurationStats partition.left
    mbSuccessStats = getDurationStats partition.right

  for_ mbFailureStats \durationStats ->
    liftEffect $ Console.log $ displayDurationStats durationStats "Failure Case"

  for_ mbSuccessStats \durationStats ->
    liftEffect $ Console.log $ displayDurationStats durationStats "Success Case"

-- TODO: Upgrade packages ref to 0.14 package set
defaultSpagoDhall :: String
defaultSpagoDhall = Array.intercalate "\n"
  [ "{ name = \"test-parser\""
  , ", dependencies = [] : List Text"
  , ", packages = https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200922/packages.dhall sha256:5edc9af74593eab8834d7e324e5868a3d258bbab75c5531d2eb770d4324a2900"
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
  , parsed :: Either ParseError (Module Unit)
  , duration :: Milliseconds
  }

parseModuleFromFile :: FilePath -> Aff ModuleResult
parseModuleFromFile path = do
  contents <- readTextFile UTF8 path
  before <- unInstant <$> liftEffect now
  let parsed = parse (lex contents)
  after <- unInstant <$> liftEffect now
  pure
    { path
    , parsed
    , duration: over2 Milliseconds sub after before
    }

parse :: TokenStream -> Either ParseError (Module Unit)
parse tokenStream =
  runFree unwrap $ Parsing.runParserT tokenStream Parser.parseModule

type DurationStats r =
  { minDuration :: { path :: FilePath, duration :: Milliseconds | r }
  , maxDuration :: { path :: FilePath, duration :: Milliseconds | r }
  , mean :: Milliseconds
  }

getDurationStats :: forall r. Array { path :: FilePath, duration :: Milliseconds | r } -> Maybe (DurationStats r)
getDurationStats res = do
  { head, tail } <- Array.uncons res
  let results = NonEmpty head tail
  pure
    { minDuration: minDuration results
    , maxDuration: maxDuration results
    , mean: mean results
    }
  where
  minDuration = foldl1 (\res1 res2 -> if res1.duration < res2.duration then res1 else res2)

  maxDuration = foldl1 (\res1 res2 -> if res1.duration > res2.duration then res1 else res2)

  mean results =
    results
      # foldMap (\{ duration: Milliseconds duration } -> Additive { duration, total: 1.0 })
      # un Additive
      # \{ duration, total } -> Milliseconds (duration / total)

displayDurationStats :: forall r. DurationStats r -> String -> String
displayDurationStats { minDuration, maxDuration, mean } title =
  Array.intercalate "\n"
    [ ""
    , "---- [ " <> title <> " Timing Information ] ----"
    , "Fastest Parse: " <> show minDuration.duration <> " at path:"
    , minDuration.path
    , ""
    , "Slowest Parse: " <> show maxDuration.duration <> " at path:"
    , maxDuration.path
    , ""
    , "Mean Parse: " <> show mean
    ]
