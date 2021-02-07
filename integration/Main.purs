module Main where

import Prelude

import Control.Monad.Free (runFree)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Filterable (partitionMap)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (unwrap)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throwException)
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
  pursFiles <- getPursFiles (tmpPath <> "/.spago")
  moduleResults <- for pursFiles parseModuleFromFile

  let
    partition = moduleResults # partitionMap \{ path, parsed } -> case parsed of
      Left parseError -> Left { path, parseError }
      Right parsedModule -> Right { path, parsedModule }

  liftEffect $ forWithIndex_ partition.left \ix failed -> do
    let
      message = Array.intercalate "\n"
        [ "---- [Error " <> show (ix + 1) <> " of " <> show (Array.length partition.left) <> "] ----"
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

-- TODO: Upgrade packages ref to 0.14 package set
defaultSpagoDhall :: String
defaultSpagoDhall = Array.intercalate "\n"
  [ "{ name = \"test-parser\""
  , ", dependencies = [] : List Text"
  , ", packages = https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200922/packages.dhall sha256:5edc9af74593eab8834d7e324e5868a3d258bbab75c5531d2eb770d4324a2900"
  , ", sources = [] : List Text"
  , "}"
  ]

getPursFiles :: FilePath -> Aff (Array FilePath)
getPursFiles root = do
  readdir root >>= foldMap \file -> do
    let path = root <> "/" <> file
    stats <- stat path
    if FS.isDirectory stats then
      getPursFiles path
    else if Regex.test pursRegex path then
      pure [ path ]
    else pure []
  where
  pursRegex = unsafeRegex "\\.purs$" noFlags

type ModuleResult =
  { path :: FilePath
  , parsed :: Either ParseError (Module Unit)
  }

parseModuleFromFile :: FilePath -> Aff ModuleResult
parseModuleFromFile path = do
  contents <- readTextFile UTF8 path
  pure
    { path
    , parsed: parse (lex contents)
    }

parse :: TokenStream -> Either ParseError (Module Unit)
parse tokenStream =
  runFree unwrap $ Parsing.runParserT tokenStream Parser.parseModule
