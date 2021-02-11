module PureScript.CST.Lexer where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Array (fold, foldMap, foldl)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char as Char
import Data.Char.Unicode as Unicode
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
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (TokenError(..))
import PureScript.CST.Layout (LayoutDelim(..), LayoutStack, insertLayout, unwindLayout)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream(..), consTokens, step)
import PureScript.CST.Types (Comment(..), LineFeed(..), ModuleName(..), SourcePos, SourceStyle(..), Token(..))

data LexResult e a
  = LexFail e String
  | LexSucc a String

newtype Lex e a = Lex (String -> LexResult e a)

instance functorLex :: Functor (Lex e) where
  map f (Lex k) = Lex \str ->
    case k str of
      LexFail a b -> LexFail a b
      LexSucc a b -> LexSucc (f a) b

instance applyLex :: Apply (Lex e) where
  apply (Lex k1) (Lex k2) = Lex \str ->
    case k1 str of
      LexFail a b -> LexFail a b
      LexSucc f str' ->
        case k2 str' of
          LexFail a b -> LexFail a b
          LexSucc x str'' ->
            LexSucc (f x) str''

instance applicativeLex :: Applicative (Lex e) where
  pure = Lex <<< LexSucc

instance bindLex :: Bind (Lex e) where
  bind (Lex k1) k = Lex \str ->
    case k1 str of
      LexFail a b -> LexFail a b
      LexSucc a str' -> do
        let (Lex k2) = k a
        k2 str'

instance altLex :: Alt (Lex e) where
  alt (Lex k1) (Lex k2) = Lex \str ->
    case k1 str of
      LexFail a str'
        | SCU.length str == SCU.length str' ->
            k2 str
        | otherwise ->
            LexFail a str'
      LexSucc a b ->
        LexSucc a b

try :: forall e a. Lex e a -> Lex e a
try (Lex k) = Lex \str ->
  case k str of
    LexFail a _ -> LexFail a str
    LexSucc a b -> LexSucc a b

regex :: forall e. e -> String -> Lex e String
regex err regexStr = Lex \str ->
  case Regex.match matchRegex str of
    Just groups
      | Just match <- NonEmptyArray.head groups ->
          LexSucc match (SCU.drop (SCU.length match) str)
    _ ->
      LexFail err str
  where
  matchRegex = unsafeRegex ("^(?:" <> regexStr <> ")") noFlags

string :: forall e. e -> String -> Lex e String
string err match = Lex \str ->
  if SCU.take (SCU.length match) str == match then
    LexSucc match (SCU.drop (SCU.length match) str)
  else
    LexFail err str

string' :: forall a e. e -> a -> String -> Lex e a
string' err res match = Lex \str ->
  if SCU.take (SCU.length match) str == match then
    LexSucc res (SCU.drop (SCU.length match) str)
  else
    LexFail err str

char :: forall e. e -> Char -> Lex e Char
char err match = Lex \str ->
  if SCU.singleton match == SCU.take 1 str then
    LexSucc match (SCU.drop 1 str)
  else
    LexFail err str

char' :: forall e a. e -> a -> Char -> Lex e a
char' err res match = Lex \str ->
  if SCU.singleton match == SCU.take 1 str then
    LexSucc res (SCU.drop 1 str)
  else
    LexFail err str

optional :: forall e a. Lex e a -> Lex e (Maybe a)
optional (Lex k) = Lex \str ->
  case k str of
    LexFail err str'
        | SCU.length str == SCU.length str' ->
            LexSucc Nothing str
        | otherwise ->
            LexFail err str'
    LexSucc a b ->
      LexSucc (Just a) b

satisfy :: forall e. e -> (Char -> Boolean) -> Lex e Char
satisfy err p = Lex \str ->
  case SCU.charAt 0 str of
    Just ch | p ch ->
      LexSucc ch (SCU.drop 1 str)
    _ ->
      LexFail err str

takeWhile :: forall e. (Char -> Boolean) -> Lex e String
takeWhile p = Lex \str -> do
  let res = SCU.takeWhile p str
  LexSucc res (SCU.drop (SCU.length res) str)

many :: forall e a. Lex e a -> Lex e (Array a)
many (Lex k) = Lex \str -> do
  let
    go acc str' =
      case k str' of
        LexFail err str''
          | SCU.length str' == SCU.length str'' ->
              LexSucc acc str'
          | otherwise ->
              LexFail err str''
        LexSucc a str'' ->
          go (Array.snoc acc a) str''
  go [] str

fail :: forall a. String -> Lex TokenError a
fail = Lex <<< LexFail <<< TokErr

lex :: String -> TokenStream
lex = init
  where
  init :: String -> TokenStream
  init str = TokenStream $ Lazy.defer \_ -> do
    let (Lex k) = leadingComments
    case k str of
      LexFail _ _ ->
        unsafeCrashWith "Leading comments can't fail."
      LexSucc leading suffix -> do
        let startPos = foldl bumpComment { line: 0, column: 0 } leading
        let stack = Tuple startPos LytRoot : Nil
        step $ go stack startPos leading suffix

  go :: LayoutStack -> SourcePos -> Array (Comment LineFeed) -> String -> TokenStream
  go stack startPos leading str = TokenStream $ Lazy.defer \_ ->
    if str == "" then
      step $ unwindLayout startPos (TokenStream $ Lazy.defer \_ -> TokenEOF startPos leading) stack
    else do
      let (Lex k) = token'
      case k str of
        LexFail error remaining -> do
          let errPos = bumpText startPos 0 (SCU.take (SCU.length str - SCU.length remaining) str)
          TokenError errPos error Nothing
        LexSucc result suffix -> do
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

  token' :: Lex TokenError { token :: Token, trailing :: Array (Comment Void), nextLeading :: Array (Comment LineFeed) }
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

leadingComments :: Lex TokenError (Array (Comment LineFeed))
leadingComments = many do
  Comment <$> comment
    <|> Space <$> spaceComment
    <|> Line <$> lineComment

trailingComments :: Lex TokenError (Array (Comment Void))
trailingComments = many do
  Comment <$> comment
    <|> Space <$> spaceComment

comment :: Lex TokenError String
comment =
  regex (TokErr "block comment") """\{-(-(?!\})|[^-]+)*-\}"""
    <|> regex (TokErr "line comment") """--[^\r\n]*"""

spaceComment :: Lex TokenError Int
spaceComment = SCU.length <$> regex (TokErr "spaces") " +"

lineComment :: Lex TokenError LineFeed
lineComment =
  string' (TokErr "newline") LF "\n"
    <|> string' (TokErr "newline") CRLF "\r\n"

token :: Lex TokenError Token
token =
  parseHole
    <|> parseModuleName
    <|> parseCharLiteral
    <|> parseStringLiteral
    <|> parseNumericLiteral
    <|> tokenLeftParen
    <|> tokenRightParen
    <|> tokenLeftBrace
    <|> tokenRightBrace
    <|> tokenLeftSquare
    <|> tokenRightSquare
    <|> tokenTick
    <|> tokenComma
  where
  parseModuleName = ado
    parts <- many (try (parseProper <* charDot))
    name <- parseName
    in name (toModuleName parts)

  parseName :: Lex _ (Maybe ModuleName -> Token)
  parseName =
    parseLower
      <|> parseUpper
      <|> parseOperator
      <|> parseSymbol

  parseLower = ado
    ident <- parseIdent
    in case _ of
      Nothing ->
        case ident of
          "forall" ->
            TokForall ASCII
          _ ->
            TokLowerName Nothing ident
      moduleName ->
        TokLowerName moduleName ident

  parseUpper :: Lex _ (Maybe ModuleName -> Token)
  parseUpper =
    flip TokUpperName <$> parseProper

  parseOperator :: Lex _ (Maybe ModuleName -> Token)
  parseOperator = ado
    symbol <- parseSymbolIdent
    in case _ of
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
            TokOperator Nothing symbol
      moduleName ->
        TokOperator moduleName symbol

  parseSymbol :: Lex _ (Maybe ModuleName -> Token)
  parseSymbol = ado
    symbol <- try (tokenLeftParen *> parseSymbolIdent <* tokenRightParen)
    in case _ of
      Nothing ->
        case symbol of
          "->" ->
            TokSymbolArrow ASCII
          "→" ->
            TokSymbolArrow Unicode
          _ ->
            TokSymbolName Nothing symbol
      moduleName ->
        TokSymbolName moduleName symbol

  parseHole = ado
    ident <- try $ charQuestionMark *> (parseIdent <|> parseProper)
    in TokHole ident

  parseProper = ado
    head <- satisfyUpper
    rest <- takeWhile isIdentChar
    in SCU.singleton head <> rest

  parseIdent = ado
    head <- satisfyIdentStart
    rest <- takeWhile isIdentChar
    in SCU.singleton head <> rest

  parseSymbolIdent = ado
    head <- satisfySymbol
    rest <- takeWhile isSymbolChar
    in SCU.singleton head <> rest

  parseCharLiteral = ado
    res <- charSingleQuote *> parseChar <* charSingleQuote
    in TokChar res.raw res.char

  parseChar = do
    ch <- charAny
    case ch of
      '\\' ->
        parseEscape
      '\'' ->
        fail "Expected character"
      _ ->
        pure { raw: SCU.singleton ch, char: ch }

  parseEscape = do
    ch <- charAny
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
    regex (TokErr "hex") "[a-fA-F0-9]{1,6}"

  parseStringLiteral =
    parseRawString <|> parseString

  parseRawString = ado
    str <- rawStringCharsRegex
    in TokRawString $ SCU.dropRight 3 $ SCU.drop 3 str

  parseString = ado
    parts <- charQuote *> many parseStringPart <* charQuote
    let { raw, string } = fold parts
    in TokString raw string

  parseStringPart =
    parseStringChars
      <|> parseStringSpaceEscape
      <|> parseStringEscape

  parseStringEscape = ado
    res <- charBackslash *> parseEscape
    in { raw: res.raw, string: SCU.singleton res.char }

  parseStringChars = ado
    raw <- stringCharsRegex
    in { raw, string: raw }

  parseStringSpaceEscape = ado
    raw <- stringSpaceEscapeRegex
    in { raw, string: "" }

  stringSpaceEscapeRegex =
    regex (TokErr "whitespace escape") """\\[ \r\n]+\\"""

  stringCharsRegex =
    regex (TokErr "string characters") """[^"\\]+"""

  rawStringCharsRegex =
    regex (TokErr "raw string characters") "\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\""

  parseNumericLiteral =
    parseHexInt <|> parseNumber

  parseHexInt = do
    raw <- hexIntPrefix *> hexIntRegex
    case Int.fromStringAs hexadecimal raw of
      Just int ->
        pure $ TokInt ("0x" <> raw) int
      Nothing ->
        fail "Hex integer literal out of range"

  parseNumber = do
    intPart <- intPartRegex
    fractionPart <- optional (try (charDot *> fractionPartRegex))
    exponentPart <- optional (charExponent *> parseExponentPart)
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

  parseExponentPart = ado
    sign <- optional parseExponentSign
    exponent <- intPartRegex
    in { sign, exponent }

  parseExponentSign =
    string (TokErr "negative") "-"
      <|> string (TokErr "positive") "+"

  intPartRegex =
    regex (TokErr "int part") """(0|[1-9][0-9_]*)"""

  fractionPartRegex =
    regex (TokErr "fraction part") """[0-9_]+"""

  hexIntRegex =
    regex (TokErr "hex int") """[a-fA-F0-9]+"""

  hexIntPrefix =
    string (TokErr "hex int prefix") "0x"

  stripUnderscores =
    String.replaceAll (Pattern "_") (Replacement "")

  charDot =
    char (TokErr "dot") '.'

  tokenLeftParen =
    char' (TokErr "left paren") TokLeftParen '('

  tokenRightParen =
    char' (TokErr "right paren") TokRightParen ')'

  tokenLeftBrace =
    char' (TokErr "left brace") TokLeftBrace '{'

  tokenRightBrace =
    char' (TokErr "right brace") TokRightBrace '}'

  tokenLeftSquare =
    char' (TokErr "left square") TokLeftSquare '['

  tokenRightSquare =
    char' (TokErr "right square") TokRightSquare ']'

  tokenTick =
    char' (TokErr "backtick") TokTick '`'

  tokenComma =
    char' (TokErr "comma") TokComma ','

  charQuestionMark =
    char (TokErr "question mark") '?'

  charSingleQuote =
    char (TokErr "single quote") '\''

  charQuote =
    char (TokErr "quote") '"'

  charBackslash =
    char (TokErr "backslash") '\\'

  charExponent =
    char (TokErr "exponent") 'e'

  charAny =
   satisfy (TokErr "char") (const true)

  satisfyUpper =
    satisfy (TokErr "upper") Unicode.isUpper

  satisfyIdentStart =
    satisfy (TokErr "identifier start") isIdentStart

  satisfySymbol =
    satisfy (TokErr "symbol") isSymbolChar

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
