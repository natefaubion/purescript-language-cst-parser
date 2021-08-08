module PureScript.CST.Lexer
  ( lex
  , lexWithState
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char as Char
import Data.Foldable (fold, foldl, foldMap)
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
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Layout (LayoutDelim(..), LayoutStack, insertLayout)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream(..), consTokens, step, unwindLayout)
import PureScript.CST.Types (Comment(..), LineFeed(..), ModuleName(..), SourcePos, SourceStyle(..), Token(..))

data LexResult e a
  = LexFail e String
  | LexSucc a String

type LexError = Unit -> ParseError

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

mkUnexpected :: String -> String
mkUnexpected str = do
  let start = String.take 6 str
  let len = String.length start
  if len == 0 then
    "end of file"
  else if len < 6 then
    start
  else
    start <> "..."

regex :: forall e. (String -> e) -> String -> Lex (Unit -> e) String
regex mkErr regexStr = Lex \str ->
  case Regex.match matchRegex str of
    Just groups
      | Just match <- NonEmptyArray.head groups ->
          LexSucc match (SCU.drop (SCU.length match) str)
    _ ->
      LexFail (\_ -> mkErr (mkUnexpected str))  str
  where
  matchRegex = unsafeRegex ("^(?:" <> regexStr <> ")") unicode

string :: forall e. (String -> e) -> String -> Lex (Unit -> e) String
string mkErr match = Lex \str ->
  if SCU.take (SCU.length match) str == match then
    LexSucc match (SCU.drop (SCU.length match) str)
  else
    LexFail (\_ -> mkErr (mkUnexpected str)) str

string' :: forall a e. (String -> e) -> a -> String -> Lex (Unit -> e) a
string' mkErr res match = Lex \str ->
  if SCU.take (SCU.length match) str == match then
    LexSucc res (SCU.drop (SCU.length match) str)
  else
    LexFail (\_ -> mkErr (mkUnexpected str)) str

char :: forall e. (String -> e) -> Char -> Lex (Unit -> e) Char
char mkErr match = Lex \str ->
  if SCU.singleton match == SCU.take 1 str then
    LexSucc match (SCU.drop 1 str)
  else
    LexFail (\_ -> mkErr (mkUnexpected str)) str

char' :: forall e a. (String -> e) -> a -> Char -> Lex (Unit -> e) a
char' mkErr res match = Lex \str ->
  if SCU.singleton match == SCU.take 1 str then
    LexSucc res (SCU.drop 1 str)
  else
    LexFail (\_ -> mkErr (mkUnexpected str)) str

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

satisfy :: forall e. (String -> e) -> (Char -> Boolean) -> Lex (Unit -> e) Char
satisfy mkErr p = Lex \str ->
  case SCU.charAt 0 str of
    Just ch | p ch ->
      LexSucc ch (SCU.drop 1 str)
    _ ->
      LexFail (\_ -> mkErr (mkUnexpected str)) str

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

fail :: forall a. ParseError -> Lex LexError a
fail = Lex <<< LexFail <<< const

lex :: String -> TokenStream
lex = lexWithState (Tuple { line: 0, column: 0 } LytRoot : Nil) { line: 0, column: 0 }

lexWithState :: LayoutStack -> SourcePos -> String -> TokenStream
lexWithState = init
  where
  init :: LayoutStack -> SourcePos -> String -> TokenStream
  init initStack initPos str = TokenStream $ Lazy.defer \_ -> do
    let (Lex k) = leadingComments
    case k str of
      LexFail _ _ ->
        unsafeCrashWith "Leading comments can't fail."
      LexSucc leading suffix -> do
        let nextPos = foldl bumpComment initPos leading
        step $ go initStack nextPos leading suffix

  go :: LayoutStack -> SourcePos -> Array (Comment LineFeed) -> String -> TokenStream
  go stack startPos leading str = TokenStream $ Lazy.defer \_ ->
    if str == "" then
      step $ unwindLayout startPos (TokenStream $ Lazy.defer \_ -> TokenEOF startPos leading) stack
    else do
      let (Lex k) = token'
      case k str of
        LexFail error remaining -> do
          let errPos = bumpText startPos 0 (SCU.take (SCU.length str - SCU.length remaining) str)
          TokenError errPos (error unit) Nothing stack
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

  token' :: Lex LexError { token :: Token, trailing :: Array (Comment Void), nextLeading :: Array (Comment LineFeed) }
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
  TokLayoutStart _ ->
    pos
  TokLayoutSep _ ->
    pos
  TokLayoutEnd _ ->
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
  Line _ n ->
    { line: line + n, column: 0 }

qualLength :: Maybe ModuleName -> Int
qualLength = maybe 0 (add 1 <<< String.length <<< unwrap)

leadingComments :: Lex LexError (Array (Comment LineFeed))
leadingComments = many do
  Comment <$> comment
    <|> Space <$> spaceComment
    <|> lineComment

trailingComments :: Lex LexError (Array (Comment Void))
trailingComments = many do
  Comment <$> comment
    <|> Space <$> spaceComment

comment :: Lex LexError String
comment =
  regex (LexExpected "block comment") """\{-(-(?!\})|[^-]+)*-\}"""
    <|> regex (LexExpected "line comment") """--[^\r\n]*"""

spaceComment :: Lex LexError Int
spaceComment = SCU.length <$> regex (LexExpected "spaces") " +"

lineComment :: Lex LexError (Comment LineFeed)
lineComment =
  (Line LF <<< String.length) <$> regex (LexExpected "newline") "\n+"
    <|> (Line CRLF <<< (_ / 2) <<< String.length) <$> regex (LexExpected "newline") "(?:\r\n)+"

token :: Lex LexError Token
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
          "_" ->
            TokUnderscore
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
          "`" ->
            TokTick
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

  parseProper =
    regex (LexExpected "proper name") "\\p{Lu}[\\p{L}0-9_']*"

  parseIdent =
    regex (LexExpected "ident") "[\\p{Ll}_][\\p{L}0-9_']*"

  parseSymbolIdent =
    regex (LexExpected "symbol") """(?:[:!#$%&*+./<=>?@\\^|~-]|(?!\p{P})\p{S})+"""

  parseCharLiteral = ado
    res <- charSingleQuote *> parseChar <* charSingleQuote
    in TokChar res.raw res.char

  parseChar = do
    ch <- charAny
    case ch of
      '\\' ->
        parseEscape
      '\'' ->
        fail $ LexExpected "character" "empty character literal"
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
        fail $ LexInvalidCharEscape $ SCU.singleton ch

  parseHexEscape = do
    esc <- hexEscapeRegex
    case Char.fromCharCode =<< Int.fromStringAs hexadecimal esc of
      Just ch ->
        pure { raw: "\\x" <> esc, char: ch }
      Nothing ->
        fail $ LexCharEscapeOutOfRange esc

  hexEscapeRegex =
    regex (LexExpected "hex") "[a-fA-F0-9]{1,6}"

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
    regex (LexExpected "whitespace escape") """\\[ \r\n]+\\"""

  stringCharsRegex =
    regex (LexExpected "string characters") """[^"\\]+"""

  rawStringCharsRegex =
    regex (LexExpected "raw string characters") "\"\"\"\"{0,2}([^\"]+\"{1,2})*[^\"]*\"\"\""

  parseNumericLiteral =
    parseHexInt <|> parseNumber

  parseHexInt = do
    raw <- hexIntPrefix *> hexIntRegex
    case Int.fromStringAs hexadecimal raw of
      Just int ->
        pure $ TokInt ("0x" <> raw) int
      Nothing ->
        fail $ LexHexOutOfRange raw

  parseNumber = do
    intPart <- intPartRegex
    fractionPart <- optional (try (charDot *> fractionPartRegex))
    exponentPart <- optional (charExponent *> parseExponentPart)
    if isNothing fractionPart && isNothing exponentPart then
      case Int.fromString (stripUnderscores intPart) of
        Just int ->
          pure $ TokInt intPart int
        Nothing ->
          fail $ LexIntOutOfRange intPart
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
          fail $ LexNumberOutOfRange raw

  parseExponentPart = ado
    sign <- optional parseExponentSign
    exponent <- intPartRegex
    in { sign, exponent }

  parseExponentSign =
    string (LexExpected "negative") "-"
      <|> string (LexExpected "positive") "+"

  intPartRegex =
    regex (LexExpected "int part") """(0|[1-9][0-9_]*)"""

  fractionPartRegex =
    regex (LexExpected "fraction part") """[0-9_]+"""

  hexIntRegex =
    regex (LexExpected "hex int") """[a-fA-F0-9]+"""

  hexIntPrefix =
    string (LexExpected "hex int prefix") "0x"

  stripUnderscores =
    String.replaceAll (Pattern "_") (Replacement "")

  charDot =
    char (LexExpected "dot") '.'

  tokenLeftParen =
    char' (LexExpected "left paren") TokLeftParen '('

  tokenRightParen =
    char' (LexExpected "right paren") TokRightParen ')'

  tokenLeftBrace =
    char' (LexExpected "left brace") TokLeftBrace '{'

  tokenRightBrace =
    char' (LexExpected "right brace") TokRightBrace '}'

  tokenLeftSquare =
    char' (LexExpected "left square") TokLeftSquare '['

  tokenRightSquare =
    char' (LexExpected "right square") TokRightSquare ']'

  tokenTick =
    char' (LexExpected "backtick") TokTick '`'

  tokenComma =
    char' (LexExpected "comma") TokComma ','

  charQuestionMark =
    char (LexExpected "question mark") '?'

  charSingleQuote =
    char (LexExpected "single quote") '\''

  charQuote =
    char (LexExpected "quote") '"'

  charBackslash =
    char (LexExpected "backslash") '\\'

  charExponent =
    char (LexExpected "exponent") 'e'

  charAny =
   satisfy (LexExpected "char") (const true)

toModuleName :: Array String -> Maybe ModuleName
toModuleName = case _ of
  [] -> Nothing
  mn -> Just $ ModuleName $ String.joinWith "." mn

moduleNameLength :: Maybe ModuleName -> Int
moduleNameLength = maybe 0 (String.length <<< un ModuleName)
