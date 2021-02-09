module PureScript.CST.Lexer where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fold, foldMap, foldl)
import Data.Array as Array
import Data.Char as Char
import Data.Char.Unicode as Unicode
import Data.Either (Either(..))
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (un, unwrap)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (TokenError(..))
import PureScript.CST.Layout (LayoutDelim(..), LayoutStack, insertLayout, unwindLayout)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream(..), consTokens, step)
import PureScript.CST.Types (Comment(..), LineFeed(..), ModuleName(..), SourcePos, SourceStyle(..), Token(..))
import Text.Parsing.StringParser (ParseError(..), Parser(..), PosString, fail, try)
import Text.Parsing.StringParser.CodeUnits (regex)
import Text.Parsing.StringParser.CodeUnits as SPSCU
import Text.Parsing.StringParser.Combinators (optionMaybe)

lex :: String -> TokenStream
lex = init <<< { str: _, pos: 0 }
  where
  init :: PosString -> TokenStream
  init str = TokenStream $ Lazy.defer \_ -> do
    let Parser k = leadingComments
    case k str of
      Left _ ->
        unsafeCrashWith "Leading comments can't fail."
      Right { result: leading, suffix } -> do
        let startPos = foldl bumpComment { line: 0, column: 0 } leading
        let stack = Tuple startPos LytRoot : Nil
        step $ go stack startPos leading suffix

  go :: LayoutStack -> SourcePos -> Array (Comment LineFeed) -> PosString -> TokenStream
  go stack startPos leading str = TokenStream $ Lazy.defer \_ ->
    if str.pos == SCU.length str.str then
      step $ unwindLayout startPos (TokenStream $ Lazy.defer \_ -> TokenEOF startPos leading) stack
    else do
      let Parser k = token'
      case k str of
        Left { pos, error: ParseError error } -> do
          let errPos = bumpText startPos 0 (String.take (pos - str.pos) (String.drop str.pos str.str))
          TokenError errPos (TokErr error) Nothing
        Right { result, suffix } -> do
          let
            endPos = bumpToken startPos result.token
            nextStart = foldl bumpComment (foldl bumpComment endPos result.trailing) result.nextLeading
            posToken =
              { range: { start: startPos, end: endPos }
              , leadingComments: leading
              , trailingComments: result.trailing
              , value: result.token
              }
            Tuple nextStack toks = insertLayout posToken nextStart stack
          step
            $ snd
            $ consTokens toks
            $ Tuple nextStart
            $ go nextStack nextStart result.nextLeading suffix

  token' :: Parser { token :: Token, trailing :: Array (Comment Void), nextLeading :: Array (Comment LineFeed) }
  token' =
    { token: _, trailing: _, nextLeading: _ }
      <$> token
      <*> trailingComments
      <*> leadingComments

bump :: Int -> Int -> SourcePos -> SourcePos
bump lines cols pos
  | lines == 0 =
      pos { column = pos.column + cols }
  | otherwise =
      { line: pos.line + lines
      , column: cols
      }

bumpToken :: SourcePos -> Token -> SourcePos
bumpToken pos@{ line, column } = case _ of
  TokLeftParen ->
    { line, column: column + 1 }
  TokRightParen ->
    { line, column: column + 1 }
  TokLeftBrace ->
    { line, column: column + 1 }
  TokRightBrace ->
    { line, column: column + 1 }
  TokLeftSquare ->
    { line, column: column + 1 }
  TokRightSquare ->
    { line, column: column + 1 }
  TokLeftArrow ASCII ->
    { line, column: column + 2 }
  TokLeftArrow Unicode ->
    { line, column: column + 1 }
  TokRightArrow ASCII ->
    { line, column: column + 2 }
  TokRightArrow Unicode ->
    { line, column: column + 1 }
  TokRightFatArrow ASCII ->
    { line, column: column + 2 }
  TokRightFatArrow Unicode ->
    { line, column: column + 1 }
  TokDoubleColon ASCII ->
    { line, column: column + 2 }
  TokDoubleColon Unicode ->
    { line, column: column + 1 }
  TokForall ASCII ->
    { line, column: column + 6 }
  TokForall Unicode ->
    { line, column: column + 1 }
  TokEquals ->
    { line, column: column + 1 }
  TokPipe ->
    { line, column: column + 1 }
  TokTick ->
    { line, column: column + 1 }
  TokDot ->
    { line, column: column + 1 }
  TokComma ->
    { line, column: column + 1 }
  TokUnderscore ->
    { line, column: column + 1 }
  TokBackslash ->
    { line, column: column + 1 }
  TokAt ->
    { line, column: column + 1 }
  TokLowerName qual name ->
    { line, column: column + qualLength qual + String.length name }
  TokUpperName qual name ->
    { line, column: column + qualLength qual + String.length name }
  TokOperator qual sym ->
    { line, column: column + qualLength qual + String.length sym }
  TokSymbolName qual sym ->
    { line, column: column + qualLength qual + String.length sym }
  TokSymbolArrow Unicode ->
    { line, column: column + 1 }
  TokSymbolArrow ASCII ->
    { line, column: column + 4 }
  TokHole hole ->
    { line, column: column + String.length hole + 1 }
  TokChar raw _ ->
    { line, column: column + String.length raw + 2 }
  TokInt raw _ ->
    { line, column: column + String.length raw }
  TokNumber raw _ ->
    { line, column: column + String.length raw }
  TokString raw _ ->
    bumpText pos 1 raw
  TokRawString raw ->
    bumpText pos 3 raw
  TokLayoutStart ->
    pos
  TokLayoutSep ->
    pos
  TokLayoutEnd ->
    pos

bumpText :: SourcePos -> Int -> String -> SourcePos
bumpText { line, column } colOffset str = go 0 0
  where
  go n ix = case SCU.indexOf' (Pattern "\n") ix str of
    Just ix' ->
      go (n + 1) (ix' + 1)
    Nothing
      | n == 0 ->
          { line, column: column + String.length str + (colOffset * 2) }
      | otherwise ->
          { line: line + n
          , column: String.length (SCU.drop ix str) + colOffset
          }

bumpComment :: forall a. SourcePos -> Comment a -> SourcePos
bumpComment pos@{ line, column } = case _ of
  Comment str ->
    bumpText pos 0 str
  Space n ->
    { line, column: column + n }
  Line _ ->
    { line: line + 1, column: 0 }

qualLength :: Maybe ModuleName -> Int
qualLength = maybe 0 (add 1 <<< String.length <<< unwrap)

leadingComments :: Parser (Array (Comment LineFeed))
leadingComments = Array.many $
  Comment <$> comment
    <|> Space <$> spaceComment
    <|> Line <$> lineComment

trailingComments :: Parser (Array (Comment Void))
trailingComments = Array.many $
  Comment <$> comment
    <|> Space <$> spaceComment

comment :: Parser String
comment =
  regex """\{-(-(?!\})|[^-]+)*-\}"""
    <|> regex """--[^\r\n]*"""

spaceComment :: Parser Int
spaceComment = SCU.length <$> SPSCU.regex " +"

lineComment :: Parser LineFeed
lineComment =
  LF <$ SPSCU.string "\n"
    <|> CRLF <$ SPSCU.string "\r\n"

token :: Parser Token
token =
  parseHole
    <|> parseModuleName []
    <|> parseLower Nothing
    <|> parseUpper Nothing
    <|> parseOperator Nothing
    <|> parseSymbol Nothing
    <|> parseCharLiteral
    <|> parseStringLiteral
    <|> parseNumericLiteral
    <|> TokLeftParen <$ SPSCU.char '('
    <|> TokRightParen <$ SPSCU.char ')'
    <|> TokLeftBrace <$ SPSCU.char '{'
    <|> TokRightBrace <$ SPSCU.char '}'
    <|> TokLeftSquare <$ SPSCU.char '['
    <|> TokRightSquare <$ SPSCU.char ']'
    <|> TokTick <$ SPSCU.char '`'
    <|> TokComma <$ SPSCU.char ','
  where
  parseModuleName init = do
    mbNext <- optionMaybe $ try (parseProper <* SPSCU.char '.')
    case mbNext of
      Nothing -> do
        let moduleName = toModuleName init
        parseLower moduleName
          <|> parseUpper moduleName
          <|> parseOperator moduleName
          <|> parseSymbol moduleName
      Just next ->
        parseModuleName (Array.snoc init next)

  parseLower moduleName = do
    ident <- parseIdent
    pure $ case moduleName of
      Nothing ->
        case ident of
          "forall" ->
            TokForall ASCII
          _ ->
            TokLowerName Nothing ident
      Just _ ->
        TokLowerName moduleName ident

  parseUpper moduleName = do
    ident <- parseProper
    pure $ TokUpperName moduleName ident

  parseOperator moduleName = do
    symbol <- parseSymbolIdent
    pure $ case moduleName of
      Nothing ->
        case symbol of
          "<-" ->
            TokLeftArrow ASCII
          "←" ->
            TokLeftArrow Unicode
          "->" ->
            TokRightArrow ASCII
          "→" ->
            TokRightArrow Unicode
          "=>" ->
            TokRightFatArrow ASCII
          "⇒" ->
            TokRightFatArrow Unicode
          "::" ->
            TokDoubleColon ASCII
          "∷" ->
            TokDoubleColon Unicode
          "∀" ->
            TokForall Unicode
          "=" ->
            TokEquals
          "." ->
            TokDot
          "\\" ->
            TokBackslash
          "|" ->
            TokPipe
          "@" ->
            TokAt
          _ ->
            TokOperator moduleName symbol
      Just _ ->
        TokOperator moduleName symbol

  parseSymbol moduleName = do
    symbol <- try (SPSCU.char '(' *> parseSymbolIdent <* SPSCU.char ')')
    pure $ case moduleName of
      Nothing ->
        case symbol of
          "->" ->
            TokSymbolArrow ASCII
          "→" ->
            TokSymbolArrow Unicode
          _ ->
            TokSymbolName Nothing symbol
      _ ->
        TokSymbolName moduleName symbol

  parseHole = do
    ident <- try $ SPSCU.char '?' *> (parseIdent <|> parseProper)
    pure $ TokHole ident

  parseProper = do
    head <- SPSCU.satisfy Unicode.isUpper
    rest <- Array.many (SPSCU.satisfy isIdentChar)
    pure $ SCU.fromCharArray $ Array.cons head rest

  parseIdent = do
    head <- SPSCU.satisfy isIdentStart
    rest <- Array.many (SPSCU.satisfy isIdentChar)
    pure $ SCU.fromCharArray $ Array.cons head rest

  parseSymbolIdent = do
    head <- SPSCU.satisfy isSymbolChar
    rest <- Array.many (SPSCU.satisfy isSymbolChar)
    pure $ SCU.fromCharArray $ Array.cons head rest

  parseCharLiteral = do
    { raw, char } <- SPSCU.char '\'' *> parseChar <* SPSCU.char '\''
    pure $ TokChar raw char

  parseChar = do
    ch <- SPSCU.anyChar
    case ch of
      '\\' ->
        parseEscape
      '\'' ->
        fail "Expected character"
      _ ->
        pure { raw: SCU.singleton ch, char: ch }

  parseEscape = do
    ch <- SPSCU.anyChar
    case ch of
      't' ->
        pure { raw: "\\t", char: '\t' }
      'r' ->
        pure { raw: "\\r", char: '\r' }
      'n' ->
        pure { raw: "\\n", char: '\n' }
      '"' ->
        pure { raw: "\\\"", char: '"' }
      '\'' ->
        pure { raw: "\\'", char: '\'' }
      '\\' ->
        pure { raw: "\\\\", char: '\\' }
      'x' ->
        parseHexEscape
      _ ->
        fail "Invalid character escape"

  parseHexEscape = do
    esc <- hexEscapeRegex
    case Char.fromCharCode =<< Int.fromStringAs hexadecimal esc of
      Just ch ->
        pure { raw: "\\x" <> esc, char: ch }
      Nothing ->
        fail "Character escape out of range"

  hexEscapeRegex =
    SPSCU.regex "[a-fA-F0-9]{1,6}"

  parseStringLiteral =
    parseRawString <|> parseString

  parseRawString = do
    string <- rawStringCharsRegex
    pure $ TokRawString $ SCU.dropRight 3 $ SCU.drop 3 string

  parseString = do
    parts <- SPSCU.char '"' *> Array.many parseStringPart <* SPSCU.char '"'
    let { raw, string } = fold parts
    pure $ TokString raw string

  parseStringPart =
    parseStringChars
      <|> parseStringSpaceEscape
      <|> parseStringEscape

  parseStringEscape = do
    { raw, char } <- SPSCU.char '\\' *> parseEscape
    pure { raw, string: SCU.singleton char }

  parseStringChars = do
    raw <- stringCharsRegex
    pure { raw, string: raw }

  parseStringSpaceEscape = do
    raw <- stringSpaceEscapeRegex
    pure { raw, string: "" }

  stringSpaceEscapeRegex =
    SPSCU.regex """\\[ \r\n]+\\"""

  stringCharsRegex =
    SPSCU.regex """[^"\\]+"""

  rawStringCharsRegex =
    SPSCU.regex "\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\""

  parseNumericLiteral =
    parseHexInt <|> parseNumber

  parseHexInt = do
    raw <- SPSCU.string "0x" *> hexIntRegex
    case Int.fromStringAs hexadecimal raw of
      Just int ->
        pure $ TokInt ("0x" <> raw) int
      Nothing ->
        fail "Hex integer literal out of range"

  parseNumber = do
    intPart <- intPartRegex
    fractionPart <- optionMaybe (try (SPSCU.char '.' *> fractionPartRegex))
    exponentPart <- optionMaybe (SPSCU.char 'e' *> parseExponentPart)
    if isNothing fractionPart && isNothing exponentPart then
      case Int.fromString intPart of
        Just int ->
          pure $ TokInt intPart int
        Nothing ->
          fail "Int literal out of range"
    else do
      let
        raw =
          intPart
            <> foldMap (\fr -> "." <> fr) fractionPart
            <> foldMap (\ex -> "e" <> fold ex.sign <> ex.exponent) exponentPart
      case Number.fromString (stripUnderscores raw) of
        Just number ->
          pure $ TokNumber raw number
        Nothing ->
          fail "Number literal out of range"

  parseExponentPart = do
    sign <- optionMaybe parseExponentSign
    exponent <- intPartRegex
    pure { sign, exponent }

  parseExponentSign =
    SPSCU.string "-"
      <|> SPSCU.string "+"

  intPartRegex =
    SPSCU.regex """(0|[1-9][0-9_]*)"""

  fractionPartRegex =
    SPSCU.regex """[0-9_]+"""

  hexIntRegex =
    SPSCU.regex """[a-fA-F0-9]+"""

  stripUnderscores =
    String.replaceAll (Pattern "_") (Replacement "")

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  String.contains (Pattern (SCU.singleton c)) ":!#$%&*+./<=>?@\\^|-~"
    || (Unicode.isSymbol c && not (Unicode.isAscii c))

isIdentStart :: Char -> Boolean
isIdentStart c = Unicode.isLower c || c == '_'

isIdentChar :: Char -> Boolean
isIdentChar c = Unicode.isAlphaNum c || c == '_' || c == '\''

toModuleName :: Array String -> Maybe ModuleName
toModuleName = case _ of
  [] -> Nothing
  mn -> Just $ ModuleName $ String.joinWith "." mn

moduleNameLength :: Maybe ModuleName -> Int
moduleNameLength = maybe 0 (String.length <<< un ModuleName)
