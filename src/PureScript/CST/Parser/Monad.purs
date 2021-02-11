module PureScript.CST.Parser.Monad
  ( Parser
  , ParserResult(..)
  , ParseError
  , PositionedError
  , runParser
  , runParser'
  , take
  , fail
  , try
  , lookAhead
  , many
  , optional
  , eof
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lazy as Z
import Data.Maybe (Maybe(..))
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Comment, LineFeed, SourcePos, SourceToken)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeBoundValue :: Type

data Queue c a b
  = Leaf (c a b)
  | Node (Queue c a UnsafeBoundValue) (Queue c UnsafeBoundValue b)

qappend :: forall c a x b. Queue c a x -> Queue c x b -> Queue c a b
qappend = unsafeCoerce Node

qsingleton :: forall c a b. c a b -> Queue c a b
qsingleton = Leaf

data UnconsView c a b x
  = UnconsDone (c a b)
  | UnconsMore (c a x) (Queue c x b)

unconsView :: forall c a b. Queue c a b -> UnconsView c a b UnsafeBoundValue
unconsView = uncons (unsafeCoerce UnconsDone) (unsafeCoerce UnconsMore)

uncons
  :: forall c a b r
   . (c a b -> r)
  -> (forall x. c a x -> Queue c x b -> r)
  -> Queue c a b
  -> r
uncons done more = case _ of
  Leaf a -> done a
  Node a b -> uncons' more a b

uncons'
  :: forall c a x b r
   . (forall z. c a z -> Queue c z b -> r)
  -> Queue c a x
  -> Queue c x b
  -> r
uncons' cons l r = case l of
  Leaf k -> cons (unsafeCoerce k) (unsafeCoerce r)
  Node l' r' -> uncons' cons l' (Node (unsafeCoerce r') (unsafeCoerce r))

type ParseError = String

type PositionedError =
  { position :: SourcePos
  , error :: ParseError
  }

newtype ParserK a b = ParserK (a -> Parser b)

data Parser a
  = Take (SourceToken -> Either ParseError a)
  | Eof (Array (Comment LineFeed) -> a)
  | Fail SourcePos ParseError
  | Alt (Parser a) (Parser a)
  | Try (Parser a)
  | LookAhead (Parser a)
  | Defer (Z.Lazy (Parser a))
  | Pure a
  | Bind (Parser UnsafeBoundValue) (Queue ParserK UnsafeBoundValue a)

instance functorParser :: Functor Parser where
  map f = case _ of
    Bind p queue ->
      Bind p (qappend queue (qsingleton (ParserK (Pure <<< f))))
    p ->
      Bind (unsafeCoerce p) (qsingleton (ParserK (Pure <<< unsafeCoerce f)))

instance applyParser :: Apply Parser where
  apply p1 p2 = do
    f <- p1
    a <- p2
    pure (f a)

instance applicativeParser :: Applicative Parser where
  pure = Pure

instance bindParser :: Bind Parser where
  bind p k = case p of
    Bind p' queue ->
      Bind p' (qappend queue (qsingleton (ParserK k)))
    _ ->
      Bind (unsafeCoerce p) (qsingleton (ParserK (unsafeCoerce k)))

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt = Alt

instance lazyParser :: Lazy (Parser a) where
  defer = Defer <<< Z.defer

take :: forall a. (SourceToken -> Either ParseError a) -> Parser a
take = Take

fail :: forall a. SourcePos -> ParseError -> Parser a
fail = Fail

try :: forall a. Parser a -> Parser a
try = Try

lookAhead :: forall a. Parser a -> Parser a
lookAhead = LookAhead

many :: forall a. Parser a -> Parser (Array a)
many p = go []
  where
  go acc = optional p >>= case _ of
    Just more ->
      go (Array.snoc acc more)
    Nothing ->
      pure acc

optional :: forall a. Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

eof :: Parser (Array (Comment LineFeed))
eof = Eof identity

data ParserResult a
  = ParseFail SourcePos ParseError (Maybe TokenStream)
  | ParseSucc a SourcePos TokenStream

data ParserStack a
  = PopAlt SourcePos TokenStream (Parser a) (ParserStack a)
  | PopNil

runParser :: forall a. TokenStream -> Parser a -> Either PositionedError a
runParser stream parser =
  case runParser' { line: 0, column: 0 } stream parser of
    ParseFail position error _ ->
      Left { position, error }
    ParseSucc res _ _ ->
      Right res

runParser' :: forall a. SourcePos -> TokenStream -> Parser a -> ParserResult a
runParser' = go PopNil
  where
  go :: ParserStack a -> SourcePos -> TokenStream -> Parser a -> ParserResult a
  go stack pos stream = case _ of
    Pure a ->
      ParseSucc a pos stream
    Bind p queue ->
      case runParser' pos stream p of
        ParseFail errPos err errStream ->
          case stack of
            PopAlt prevPos prevStream prevParser prevStack ->
              go prevStack prevPos prevStream prevParser
            _ ->
              ParseFail errPos err errStream
        ParseSucc a nextPos nextStream ->
          case unconsView queue of
            UnconsDone (ParserK k) ->
              go stack nextPos nextStream (k a)
            UnconsMore (ParserK k) queue' ->
              go stack nextPos nextStream (Bind (k a) queue')
    Take k ->
      case TokenStream.step stream of
        TokenError errPos _ errStream ->
          ParseFail errPos "Failed to parse token" errStream
        TokenEOF errPos _ ->
          case stack of
            PopAlt prevPos prevStream prevParser prevStack ->
              go prevStack prevPos prevStream prevParser
            _ ->
              ParseFail errPos "Unexpected EOF" Nothing
        TokenCons tok nextPos nextStream ->
          case k tok of
            Left err ->
              case stack of
                PopAlt prevPos prevStream prevParser prevStack ->
                  go prevStack prevPos prevStream prevParser
                _ ->
                  ParseFail pos err (Just nextStream)
            Right a ->
              ParseSucc a nextPos nextStream
    Eof k ->
      case TokenStream.step stream of
        TokenError errPos _ errStream ->
          ParseFail errPos "Failed to parse token" errStream
        TokenEOF eofPos comments ->
          ParseSucc (k comments) eofPos stream
        TokenCons tok _ _ ->
          case stack of
            PopAlt prevPos prevStream prevParser prevStack ->
              go prevStack prevPos prevStream prevParser
            _ ->
              ParseFail tok.range.start "Expected EOF" (Just stream)
    Fail errPos err ->
      case stack of
        PopAlt prevPos prevStream prevParser prevStack ->
          go prevStack prevPos prevStream prevParser
        _ ->
          ParseFail errPos err (Just stream)
    Alt a b ->
      go (PopAlt pos stream b stack) pos stream a
    Try p ->
      case runParser' pos stream p of
        err@(ParseFail _ _ _) ->
          case stack of
            PopAlt prevPos prevStream prevParser prevStack ->
              go prevStack prevPos prevStream prevParser
            _ ->
              err
        succ ->
          succ
    LookAhead p ->
      case runParser' pos stream p of
        err@(ParseFail _ _ _) ->
          case stack of
            PopAlt prevPos prevStream prevParser prevStack ->
              go prevStack prevPos prevStream prevParser
            _ ->
              err
        ParseSucc a _ _ ->
          ParseSucc a pos stream
    Defer z ->
      go stack pos stream (Z.force z)
