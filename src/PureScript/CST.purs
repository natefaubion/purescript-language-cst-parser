module PureScript.CST
  ( RecoveredParserResult(..)
  , PartialModule(..)
  , parseModule
  , parsePartialModule
  , parseImportDecl
  , parseDecl
  , parseExpr
  , parseType
  , parseBinder
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Lazy as Z
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser (Recovered, parseModuleBody, parseModuleHeader)
import PureScript.CST.Parser as Parser
import PureScript.CST.Parser.Monad (Parser, ParserResult(..), PositionedError, fromParserResult, initialParserState, runParser, runParser')
import PureScript.CST.Types (ImportDecl, ModuleHeader, PSBinder, PSDeclaration, PSExpr, PSModule(..), PSType)
import Unsafe.Coerce (unsafeCoerce)

data RecoveredParserResult f
  = ParseSucceeded (f Void)
  | ParseSucceededWithErrors (Recovered f) (NonEmptyArray PositionedError)
  | ParseFailed PositionedError

toRecoveredParserResult
  :: forall f
   . Either PositionedError (Tuple (Recovered f) (Array PositionedError))
  -> RecoveredParserResult f
toRecoveredParserResult = case _ of
  Right (Tuple res errors)
    | Just nea <- NonEmptyArray.fromArray errors ->
        ParseSucceededWithErrors res nea
    | otherwise ->
        ParseSucceeded ((unsafeCoerce :: Recovered f -> f Void) res)
  Left err ->
    ParseFailed err

runRecoveredParser :: forall a. Parser (Recovered a) -> String -> RecoveredParserResult a
runRecoveredParser p = toRecoveredParserResult <<< flip runParser p <<< lex

parseModule :: String -> RecoveredParserResult PSModule
parseModule = runRecoveredParser Parser.parseModule

parseImportDecl :: String -> RecoveredParserResult ImportDecl
parseImportDecl = runRecoveredParser Parser.parseImportDecl

parseDecl :: String -> RecoveredParserResult PSDeclaration
parseDecl = runRecoveredParser Parser.parseDecl

parseExpr :: String -> RecoveredParserResult PSExpr
parseExpr = runRecoveredParser Parser.parseExpr

parseType :: String -> RecoveredParserResult PSType
parseType = runRecoveredParser Parser.parseType

parseBinder :: String -> RecoveredParserResult PSBinder
parseBinder = runRecoveredParser Parser.parseBinder

newtype PartialModule e = PartialModule
  { header :: ModuleHeader e
  , full :: Z.Lazy (RecoveredParserResult PSModule)
  }

parsePartialModule :: String -> RecoveredParserResult PartialModule
parsePartialModule src =
  toRecoveredParserResult $ case runParser' (initialParserState (lex src)) parseModuleHeader of
    ParseSucc header state -> do
      let
        res = PartialModule
          { header
          , full: Z.defer \_ ->
              toRecoveredParserResult $ fromParserResult $ runParser' state do
                body <- parseModuleBody
                pure $ PSModule { header, body }
          }
      Right $ Tuple res state.errors
    ParseFail error position _ _ ->
      Left { error, position }
