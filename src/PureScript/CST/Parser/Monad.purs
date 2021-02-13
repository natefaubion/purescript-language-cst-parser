module PureScript.CST.Parser.Monad
  ( Parser
  , ParserResult(..)
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
import Control.Lazy (class Lazy, fix)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lazy as Z
import Data.Maybe (Maybe(..))
import PureScript.CST.Errors (ParseError(..))
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
many p = fix \go ->
  Array.cons <$> p <*> go
    <|> pure []

optional :: forall a. Parser a -> Parser (Maybe a)
optional p =
  Just <$> p
    <|> pure Nothing

eof :: Parser (Array (Comment LineFeed))
eof = Eof identity

runParser :: forall a. TokenStream -> Parser a -> Either PositionedError a
runParser stream parser =
  case runParser' stream parser of
    ParseFail error position _ _ ->
      Left { position, error }
    ParseSucc res _ _ _ ->
      Right res

data ParserResult a
  = ParseFail ParseError SourcePos Boolean (Maybe TokenStream)
  | ParseSucc a SourcePos Boolean TokenStream

data ParserStack a
  = StkNil
  | StkAlt (ParserStack a) ParserState (Parser a)
  | StkTry (ParserStack a) ParserState
  | StkLookAhead (ParserStack a) ParserState
  | StkBinds (ParserStack a) (ParserBinds a)

type ParserBinds =
  Queue ParserK UnsafeBoundValue

type ParserState =
  { consumed :: Boolean
  , position :: SourcePos
  , stream :: TokenStream
  }

data FailUnwind a
  = FailStop (ParserResult a)
  | FailAlt (ParserStack a) ParserState (Parser a)

data SuccUnwind a
  = SuccStop (ParserResult a)
  | SuccBinds (ParserStack a) ParserState (ParserBinds a)

runParser' :: forall a. TokenStream -> Parser a -> ParserResult a
runParser' = \stream parser ->
  (unsafeCoerce :: ParserResult UnsafeBoundValue -> ParserResult a) $
    go StkNil
      { consumed: false
      , position: { line: 0, column: 0 }
      , stream
      }
      (unsafeCoerce parser)
  where
  go :: ParserStack UnsafeBoundValue -> ParserState -> Parser UnsafeBoundValue -> ParserResult UnsafeBoundValue
  go stack state = case _ of
    Alt a b ->
      go (StkAlt stack state b) (state { consumed = false }) a
    Try a ->
      go (StkTry stack state) state a
    LookAhead a ->
      go (StkLookAhead stack state) state a
    Bind p binds ->
      go (StkBinds stack binds) state p
    p@(Pure a) ->
      case unwindSucc a state stack of
        SuccBinds prevStack prevState queue ->
          case unconsView queue of
            UnconsDone (ParserK k) ->
              go prevStack prevState (k a)
            UnconsMore (ParserK k) nextQueue ->
              go (StkBinds prevStack nextQueue) prevState (k a)
        SuccStop res ->
          res
    p@(Fail errPos err) ->
      case unwindFail err errPos state stack of
        FailAlt prevStack prevState prev ->
          go prevStack prevState prev
        FailStop res ->
          res
    Take k ->
      case TokenStream.step state.stream of
        TokenError errPos err errStream ->
          ParseFail err errPos state.consumed errStream
        TokenEOF errPos _ ->
          go stack state (Fail errPos UnexpectedEof)
        TokenCons tok nextPos nextStream ->
          case k tok of
            Left err ->
              go stack state (Fail tok.range.start err)
            Right a ->
              go stack { consumed: true, position: nextPos, stream: nextStream } (Pure a)
    Eof k ->
      case TokenStream.step state.stream of
        TokenError errPos err errStream ->
          ParseFail err errPos state.consumed errStream
        TokenEOF eofPos comments ->
          go stack (state { consumed = true, position = eofPos }) (Pure (k comments))
        TokenCons tok _ _ ->
          go stack state (Fail tok.range.start (ExpectedEof tok.value))
    Defer z ->
      go stack state (Z.force z)

  unwindFail :: ParseError -> SourcePos -> ParserState -> ParserStack UnsafeBoundValue -> FailUnwind UnsafeBoundValue
  unwindFail err errPos state = case _ of
    StkNil ->
      FailStop (ParseFail err errPos state.consumed (Just state.stream))
    StkAlt prevStack prevState prev ->
      if state.consumed then
        unwindFail err errPos state prevStack
      else
        FailAlt prevStack prevState prev
    StkTry prevStack prevState ->
      unwindFail err errPos (state { consumed = prevState.consumed }) prevStack
    StkLookAhead prevStack prevState ->
      unwindFail err errPos prevState prevStack
    StkBinds prevStack _ ->
      unwindFail err errPos state prevStack

  unwindSucc :: UnsafeBoundValue -> ParserState -> ParserStack UnsafeBoundValue -> SuccUnwind UnsafeBoundValue
  unwindSucc a state = case _ of
    StkNil ->
      SuccStop (ParseSucc a state.position state.consumed state.stream)
    StkAlt prevStack _ _ ->
      unwindSucc a state prevStack
    StkTry prevStack _ ->
      unwindSucc a state prevStack
    StkLookAhead prevStack prevState ->
      unwindSucc a prevState prevStack
    StkBinds prevStack queue ->
      SuccBinds prevStack state queue
