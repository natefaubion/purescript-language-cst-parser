module PureScript.CST.TokenStream where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char.Unicode as Unicode
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype, un)
import Data.String (CodePoint, Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import PureScript.CST.Errors (TokenError)
import PureScript.CST.Types (Comment, LineFeed, ModuleName(..), SourcePos, SourceStyle(..), SourceToken, Token(..))

newtype TokenStream = TokenStream (Lazy TokenStep)

derive instance newtypeTokenStream :: Newtype TokenStream _

data TokenStep
  = TokenEOF SourcePos (Array (Comment LineFeed))
  | TokenError SourcePos TokenError (Maybe TokenStream)
  | TokenCons SourceToken SourcePos TokenStream

bump :: Int -> Int -> SourcePos -> SourcePos
bump lines cols pos
  | lines == 0 =
      pos { column = pos.column + cols }
  | otherwise =
      { line: pos.line + lines
      , column: cols
      }

-- lex :: String -> TokenStream
-- lex = go { line: 0, column: 0 }
--   where
--   next :: Token -> SourcePos -> String -> TokenStream
--   next token pos inp =
--     ?next

--   error :: String -> SourcePos -> String -> TokenStream
--   error ch pos inp =
--     ?error

--   go :: SourcePos -> String -> TokenStream
--   go pos inp = case String.take 1 inp of
--     "(" ->
--       leftParen pos (String.drop 1 inp)
--     ")" ->
--       next TokRightParen pos (String.drop 1 inp)
--     "{" ->
--       next TokLeftBrace pos (String.drop 1 inp)
--     "}" ->
--       next TokRightBrace pos (String.drop 1 inp)
--     "[" ->
--       next TokLeftSquare pos (String.drop 1 inp)
--     "]" ->
--       next TokRightSquare pos (String.drop 1 inp)
--     "`" ->
--       next TokTick pos (String.drop 1 inp)
--     "," ->
--       next TokComma pos (String.drop 1 inp)
--     "?" ->
--       hole pos (String.drop 1 inp)
--     "\"" ->
--       string pos (String.drop 1 inp)
--     "'" ->
--       char pos (String.drop 1 inp)
--     ch | expectChar isIdentStart ch ->
--       lower [] ch pos (String.drop 1 inp)
--     ch | expectChar Unicode.isUpper ch ->
--       upper [] ch pos (String.drop 1 inp)
--     ch | expectChar isSymbolChar ch ->
--       operator [] ch pos (String.drop 1 inp)
--     ch | expectChar Unicode.isDigit ch ->
--       number ch pos (String.drop 1 inp)
--     ch ->
--       error ch pos (String.drop 1 inp)
--     -- "∷" ->
--     --   orOperator1 (TokDoubleColon Unicode) "∷" pos (String.drop 1 inp)
--     -- "←" ->
--     --   orOperator1 (TokLeftArrow Unicode) "←" pos (String.drop 1 inp)
--     -- "→" ->
--     --   orOperator1 (TokRightArrow Unicode) "→" pos (String.drop 1 inp)
--     -- "⇒" ->
--     --   orOperator1 (TokRightFatArrow Unicode) "⇒" pos (String.drop 1 inp)
--     -- "∀" ->
--     --   orOperator1 (TokForall Unicode) "∀" pos (String.drop 1 inp)
--     -- "|" ->
--     --   orOperator1 TokPipe "|" pos (String.drop 1 inp)
--     -- "." ->
--     --   orOperator1 TokDot "." pos (String.drop 1 inp)
--     -- "\\" ->
--     --   orOperator1 TokBackslash "\\" pos (String.drop 1 inp)

--   leftParen :: SourcePos -> String -> TokenStream
--   leftParen pos inp1 =
--     case spanWhile isSymbolChar inp1 of
--       "" /\ _ ->
--         next TokLeftParen pos inp1
--       op /\ inp2
--         | String.take 1 inp2 == ")" ->
--             case op of
--               "→" ->
--                 next (TokSymbolArrow Unicode) pos (String.drop 1 inp2)
--               "->" ->
--                 next (TokSymbolArrow ASCII) pos (String.drop 1 inp2)
--               _ ->
--                 next (TokSymbolName Nothing op) pos (String.drop 1 inp2)
--         | otherwise ->
--             next TokLeftParen pos inp1

--   hole :: SourcePos -> String -> TokenStream
--   hole pos inp1 =
--     case spanWhile isIdentChar inp1 of
--       "" /\ _ ->
--         next (TokOperator Nothing "?") pos inp1
--       cs /\ inp2 ->
--         next (TokHole cs) pos inp2

--   lower :: Array String -> String -> SourcePos -> String -> TokenStream
--   lower qual ch pos inp1 = do
--     let chs /\ inp2 = spanWhile isIdentChar inp1
--     case qual, ch <> chs of
--       [], "_" ->
--         next TokUnderscore pos inp1
--       [], "forall" ->
--         next (TokForall ASCII) pos inp2
--       _, all ->
--         next (TokLowerName (toModuleName qual) all) pos inp2

--   upper :: Array String -> String -> SourcePos -> String -> TokenStream
--   upper qual ch pos inp1 = do
--     let chs /\ inp2 = spanWhile isIdentChar inp1
--     let name = ch <> chs
--     let p1 /\ inp3 = splitAt 1 inp2
--     case p1 of
--       "." -> do
--         let qual' = Array.snoc qual name
--         let p2 /\ inp4 = splitAt 1 inp3
--         case p2 of
--           "" ->
--             error "EOF" pos inp3
--           "(" ->
--             symbol qual' pos inp4
--           _ | expectChar Unicode.isUpper p2 ->
--                 upper qual' p2 pos inp4
--             | expectChar isIdentStart p2 ->
--                 lower qual' p2 pos inp4
--             | expectChar isSymbolChar p2 ->
--                 operator qual' p2 pos inp4
--             | otherwise ->
--                 -- TODO
--                 error "Unexpected lexeme" pos inp4
--       _ ->
--         next (TokUpperName (toModuleName qual) name) pos inp2

--   operator :: Array String -> String -> SourcePos -> String -> TokenStream
--   operator qual ch pos inp1 = do
--     let chs /\ inp2 = spanWhile isSymbolChar inp1
--     -- TODO: Reserved symbols
--     next (TokOperator (toModuleName qual) (ch <> chs)) pos inp2

--   symbol :: Array String -> SourcePos -> String -> TokenStream
--   symbol qual pos inp1 =
--     case spanWhile isSymbolChar inp1 of
--       "" /\ _ ->
--         error "Expected symbol" pos inp1
--       op /\ inp2 -> do
--         let mn = toModuleName qual
--         let ch /\ inp3 = splitAt 1 inp2
--         case ch of
--           ")" ->
--             next (TokSymbolName mn op) pos inp3
--           _ ->
--             error ("Expected )") (bump 0 (moduleNameLength mn + String.length op + 2) pos) inp3

--   char :: SourcePos -> String -> TokenStream
--   char pos inp1 =
--     case splitAt 1 inp1 of
--       "" /\ _ ->
--         error "EOF" pos inp1
--       "\\"

--   escape :: String -> _
--   escape inp1 = do
--     let ch /\ inp2 = splitAt1 inp1
--     case ch of
--       "t" -> "\\t" /\ "\t" /\ inp2
--       "r" -> "\\r" /\ "\r" /\ inp2
--       "n" -> "\\n" /\ "\n" /\ inp2
--       "'" -> "\\'" /\ "'" /\ inp2
--       "\"" -> "\\\"" /\ "\"" /\ inp2
--       "\\" -> "\\\\" /\ "\\" /\ inp2
--       "x" -> do
--         let
--           go n acc = case _ of

--   string :: SourcePos -> String -> TokenStream
--   string = ?string

--   number :: String -> SourcePos -> String -> TokenStream
--   number = ?number

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  String.contains (Pattern (SCU.singleton c)) ":!#$%&*+./<=>?@\\^|-~"
    || (Unicode.isSymbol c && not (Unicode.isAscii c))

isIdentStart :: Char -> Boolean
isIdentStart c = Unicode.isLower c || c == '_'

isIdentChar :: Char -> Boolean
isIdentChar c = Unicode.isAlphaNum c || c == '_' || c == '\''

isDigitChar :: Char -> Boolean
isDigitChar c = c >= '0' && c <= '9'

isNumberChar :: Char -> Boolean
isNumberChar c = (c >= '0' && c <= '9') || c == '_'

isNormalStringChar :: Char -> Boolean
isNormalStringChar c = c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'

isStringGapChar :: Char -> Boolean
isStringGapChar c = c == ' ' || c == '\r' || c == '\n'

isLineFeed :: Char -> Boolean
isLineFeed c = c == '\r' || c == '\n'

expectChar :: (Char -> Boolean) -> String -> Boolean
expectChar p str = SCU.length str == SCU.length (SCU.takeWhile p str)

spanWhile :: (Char -> Boolean) -> String -> Tuple String String
spanWhile p str = do
  let head = SCU.takeWhile p str
  Tuple head (SCU.drop (SCU.length head) str)

splitAt :: Int -> String -> Tuple String String
splitAt n str = Tuple (SCU.take n str) (SCU.drop n str)

toModuleName :: Array String -> Maybe ModuleName
toModuleName = case _ of
  [] -> Nothing
  mn -> Just $ ModuleName $ String.joinWith "." mn

moduleNameLength :: Maybe ModuleName -> Int
moduleNameLength = maybe 0 (String.length <<< un ModuleName)

charEscapePattern :: String
charEscapePattern =
  """(?:t|r|n|'|"|\\|x(?:[a-fA-F0-9]{1,6}))"""

charPattern :: String
charPattern =
  """^(?:\\""" <> charEscapePattern <> """|[^'])"""

whiteSpaceEscapePattern :: String
whiteSpaceEscapePattern =
  """(?:[ \r\n]+\\)"""

stringChunkPattern :: String
stringChunkPattern =
  """^(""" <> whiteSpaceEscapePattern <> """|""" <> charEscapePattern <> """|[^"]+)"""

rawStringChunkPattern :: String
rawStringChunkPattern =
  """^((?:[^"]+|"(?!"")|""(?!"))*)"""

numberPattern :: String
numberPattern =
  """^(\.[0-9_]+)(?:e((?:\+|-)(?:0|[1-9][0-9_]*)))?"""

intPattern :: String
intPattern =
  """^[0-9_]*"""

hexPattern :: String
hexPattern =
  """^x[0-9a-fA-F]+"""

-- data Match
--   | Match Token String
--   | MatchError SourcePos TokenError
--   | NoMatch

-- newtype Matcher = Matcher (SourcePos -> String -> Match)

-- matchers :: Array Matcher
-- matchers =
--   [ matchString ")" TokRightParen
--   , matchString "{" TokLeftBrace
--   , matchString "}" TokRightBrace
--   , matchString "[" TokLeftSquare
--   , matchString "]" TokRightSquare

--   ]

-- matchString :: String -> Token -> Matcher
-- matchString prefix tok = Matcher \_ inp ->
--   case stripPrefix (Pattern prefix) inp of
--     Just next -> Match tok next
--     Nothing -> NoMatch

-- matchRegex :: Regex -> (String -> Array (Maybe String) -> Token) -> Matcher
-- matchRegex reg k = Matcher \_ inp ->
--   case Regex.match reg inp of
--     Just nea ->
--       case NonEmptyArray.head nead of
--         Just all ->
--           Match (k all (NonEmptyArray.tail nea)) (CodeUnits.drop (CodeUnits.length all) inp)
--         Nothing ->
--           NoMatch
--     Nothing ->
--       NoMatch
