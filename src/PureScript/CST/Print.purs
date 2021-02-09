module PureScript.CST.Print where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import PureScript.CST.Types (ModuleName, SourceStyle(..), Token(..))

data TokenOption
  = ShowLayout
  | HideLayout

printToken :: Token -> String
printToken = printTokenWithOption HideLayout

printTokenWithOption :: TokenOption -> Token -> String
printTokenWithOption option = case _ of
  TokLeftParen ->
    "("
  TokRightParen ->
    ")"
  TokLeftBrace ->
    "{"
  TokRightBrace ->
    "}"
  TokLeftSquare ->
    "["
  TokRightSquare ->
    "]"
  TokLeftArrow style ->
    case style of
      ASCII -> "<-"
      Unicode -> "←"
  TokRightArrow style ->
    case style of
      ASCII -> "->"
      Unicode -> "→"
  TokRightFatArrow style ->
    case style of
      ASCII -> "=>"
      Unicode -> "⇒"
  TokDoubleColon style ->
    case style of
      ASCII -> "::"
      Unicode -> "∷"
  TokForall style ->
    case style of
      ASCII -> "forall"
      Unicode -> "∀"
  TokEquals ->
    "="
  TokPipe ->
    "|"
  TokTick ->
    "`"
  TokDot ->
    "."
  TokComma ->
    ","
  TokUnderscore ->
    "_"
  TokBackslash ->
    "\\"
  TokAt ->
    "@"
  TokLowerName moduleName name ->
    printQualified moduleName name
  TokUpperName moduleName name ->
    printQualified moduleName name
  TokOperator moduleName name ->
    printQualified moduleName name
  TokSymbolName moduleName name ->
    printQualified moduleName name
  TokSymbolArrow style ->
    case style of
      ASCII -> "(->)"
      Unicode -> "(→)"
  TokHole name ->
    "?" <> name
  TokChar raw _ ->
    "'" <> raw <> "'"
  TokString raw _ ->
    "\"" <> raw <> "\""
  TokRawString raw ->
    "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _ ->
    raw
  TokNumber raw _ ->
    raw
  TokLayoutStart ->
    case option of
      ShowLayout -> "{"
      HideLayout -> ""
  TokLayoutSep ->
    case option of
      ShowLayout -> ";"
      HideLayout -> ""
  TokLayoutEnd ->
    case option of
      ShowLayout -> "}"
      HideLayout -> ""

printQualified :: Maybe ModuleName -> String -> String
printQualified moduleName name = case moduleName of
  Nothing -> name
  Just mn -> unwrap mn <> "." <> name
