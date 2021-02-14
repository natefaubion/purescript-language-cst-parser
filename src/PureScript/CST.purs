module PureScript.CST
  ( RecoveredParserResult(..)
  , PartialModule(..)
  , parseModule
  , parsePartialModule
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
import PureScript.CST.Parser.Monad (ParserResult(..), PositionedError, fromParserResult, initialParserState, runParser, runParser')
import PureScript.CST.Types (Module(..), ModuleHeader)
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

parseModule :: String -> RecoveredParserResult Module
parseModule src = toRecoveredParserResult $ runParser (lex src) Parser.parseModule

newtype PartialModule e = PartialModule
  { header :: ModuleHeader e
  , full :: Z.Lazy (RecoveredParserResult Module)
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
                pure $ Module { header, body }
          }
      Right $ Tuple res state.errors
    ParseFail error position _ _ ->
      Left { error, position }
