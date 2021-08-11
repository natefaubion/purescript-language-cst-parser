module PureScript.CST.Parser.Monad
  ( Parser
  , ParserState
  , ParserResult(..)
  , PositionedError
  , Recovery(..)
  , initialParserState
  , fromParserResult
  , runParser
  , runParser'
  , take
  , fail
  , try
  , lookAhead
  , many
  , optional
  , eof
  , recover
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Lazy (class Lazy)
import Control.Monad.ST.Class (liftST)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either(..))
import Data.Lazy as Z
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Unsafe (unsafePerformEffect)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Comment, LineFeed, SourceToken, SourcePos)
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

data Recovery a = Recovery a SourcePos TokenStream

derive instance functorRecovery :: Functor Recovery

type FoldBox a b s =
  { init :: Unit -> s
  , step :: s -> a -> s
  , done :: s -> b
  }

foreign import data Fold :: Type -> Type -> Type

mkFold :: forall a b s. FoldBox a b s -> Fold a b
mkFold = unsafeCoerce

unFold :: forall r a b. (forall s. FoldBox a b s -> r) -> Fold a b -> r
unFold = unsafeCoerce

foldMaybe :: forall a. Fold a (Maybe a)
foldMaybe = mkFold
  { init: const Nothing
  , step: const Just
  , done: identity
  }

foldArray :: forall a. Fold a (Array a)
foldArray = mkFold
  { init: \_ ->
      unsafePerformEffect $ liftST STArray.new
  , step: \arr a ->
      unsafePerformEffect $ liftST do
        _ <- STArray.push a arr
        pure arr
  , done:
      unsafePerformEffect <<< liftST <<< STArray.unsafeFreeze
  }

data Parser a
  = Take (SourceToken -> Either ParseError a)
  | Eof (Tuple SourcePos (Array (Comment LineFeed)) -> a)
  | Fail SourcePos ParseError
  | Alt (Parser a) (Parser a)
  | Try (Parser a)
  | LookAhead (Parser a)
  | Defer (Z.Lazy (Parser a))
  | Recover (PositionedError -> TokenStream -> Recovery a) (Parser a)
  | Iter (Fold UnsafeBoundValue a) (Parser UnsafeBoundValue)
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

iter :: forall a b. Fold a b -> Parser a -> Parser b
iter a b = Iter (unsafeCoerce a) (unsafeCoerce b)

lookAhead :: forall a. Parser a -> Parser a
lookAhead = LookAhead

many :: forall a. Parser a -> Parser (Array a)
many = iter foldArray

optional :: forall a. Parser a -> Parser (Maybe a)
optional = iter foldMaybe

eof :: Parser (Tuple SourcePos (Array (Comment LineFeed)))
eof = Eof identity

recover :: forall a. (PositionedError -> TokenStream -> Recovery a) -> Parser a -> Parser a
recover = Recover

runParser :: forall a. TokenStream -> Parser a -> Either PositionedError (Tuple a (Array PositionedError))
runParser stream = fromParserResult <<< runParser' (initialParserState stream)

fromParserResult :: forall a. ParserResult a -> Either PositionedError (Tuple a (Array PositionedError))
fromParserResult = case _ of
  ParseFail error position _ _ ->
    Left { position, error }
  ParseSucc res { errors } ->
    Right (Tuple res errors)

data ParserResult a
  = ParseFail ParseError SourcePos ParserState (Maybe TokenStream)
  | ParseSucc a ParserState

data ParserStack a
  = StkNil
  | StkAlt (ParserStack a) ParserState (Parser a)
  | StkTry (ParserStack a) ParserState
  | StkLookAhead (ParserStack a) ParserState
  | StkBinds (ParserStack a) (ParserBinds a)
  | StkRecover (ParserStack a) ParserState (PositionedError -> TokenStream -> Recovery a)

type ParserBinds =
  Queue ParserK UnsafeBoundValue

type ParserState =
  { consumed :: Boolean
  , errors :: Array PositionedError
  , position :: SourcePos
  , stream :: TokenStream
  }

initialParserState :: TokenStream -> ParserState
initialParserState stream =
  { consumed: false
  , errors: []
  , position: { line: 0, column: 0 }
  , stream
  }

data FailUnwind a
  = FailStop (ParserResult a)
  | FailAlt (ParserStack a) ParserState (Parser a)
  | FailRecover (ParserStack a) ParserState a

data SuccUnwind a
  = SuccStop (ParserResult a)
  | SuccBinds (ParserStack a) ParserState (ParserBinds a)

runParser' :: forall a. ParserState -> Parser a -> ParserResult a
runParser' = \state parser ->
  (unsafeCoerce :: ParserResult UnsafeBoundValue -> ParserResult a) $
    go StkNil state (unsafeCoerce parser)
  where
  go :: ParserStack UnsafeBoundValue -> ParserState -> Parser UnsafeBoundValue -> ParserResult UnsafeBoundValue
  go stack state@{ errors } = case _ of
    Alt a b ->
      go (StkAlt stack state b) (state { consumed = false }) a
    Try a ->
      go (StkTry stack state) state a
    LookAhead a ->
      go (StkLookAhead stack state) state a
    Bind p binds ->
      go (StkBinds stack binds) state p
    Pure a ->
      case unwindSucc a state stack of
        SuccBinds prevStack prevState queue ->
          case unconsView queue of
            UnconsDone (ParserK k) ->
              go prevStack prevState (k a)
            UnconsMore (ParserK k) nextQueue ->
              go (StkBinds prevStack nextQueue) prevState (k a)
        SuccStop res ->
          res
    Fail errPos err ->
      case unwindFail err errPos state stack of
        FailAlt prevStack prevState prev ->
          go prevStack prevState prev
        FailRecover prevStack prevState a ->
          go prevStack prevState (Pure a)
        FailStop res ->
          res
    Take k ->
      case TokenStream.step state.stream of
        TokenError errPos err errStream _ ->
          ParseFail err errPos state errStream
        TokenEOF errPos _ ->
          go stack state (Fail errPos UnexpectedEof)
        TokenCons tok nextPos nextStream _ ->
          case k tok of
            Left err ->
              go stack state (Fail tok.range.start err)
            Right a ->
              go stack { consumed: true, errors, position: nextPos, stream: nextStream } (Pure a)
    Eof k ->
      case TokenStream.step state.stream of
        TokenError errPos err errStream _ ->
          ParseFail err errPos state errStream
        TokenEOF eofPos comments ->
          go stack (state { consumed = true, position = eofPos }) (Pure (k (Tuple eofPos comments)))
        TokenCons tok _ _ _ ->
          go stack state (Fail tok.range.start (ExpectedEof tok.value))
    Iter f p -> do
      let
        Tuple state' a = f # unFold \{ init, step, done } -> do
          let
            iter acc state' = case runParser' (state' { consumed = false }) p of
              ParseSucc a state'' ->
                iter (step acc a) state''
              ParseFail err errPos state'' _
                | state''.consumed ->
                    Tuple state'' (Fail errPos err)
                | otherwise ->
                    Tuple state' (Pure (done acc))
          iter (init unit) state
      go stack state' a
    Defer z ->
      go stack state (Z.force z)
    Recover k p ->
      go (StkRecover stack state k) (state { consumed = false }) p

  unwindFail :: ParseError -> SourcePos -> ParserState -> ParserStack UnsafeBoundValue -> FailUnwind UnsafeBoundValue
  unwindFail error position state@{ errors } = case _ of
    StkNil ->
      FailStop (ParseFail error position state (Just state.stream))
    StkAlt prevStack prevState prev ->
      if state.consumed then
        unwindFail error position state prevStack
      else
        FailAlt prevStack prevState prev
    StkTry prevStack prevState ->
      unwindFail error position (state { consumed = prevState.consumed }) prevStack
    StkRecover prevStack prevState k ->
      if state.consumed then do
        let posError = { error, position }
        let nextErrors = Array.snoc errors posError
        let (Recovery a nextPos nextStream) = k posError prevState.stream
        FailRecover prevStack
          { consumed: true
          , errors: nextErrors
          , position: nextPos
          , stream: nextStream
          }
          a
      else
        unwindFail error position (state { consumed = state.consumed || prevState.consumed }) prevStack
    StkLookAhead prevStack prevState ->
      unwindFail error position prevState prevStack
    StkBinds prevStack _ ->
      unwindFail error position state prevStack

  unwindSucc :: UnsafeBoundValue -> ParserState -> ParserStack UnsafeBoundValue -> SuccUnwind UnsafeBoundValue
  unwindSucc a state = case _ of
    StkNil ->
      SuccStop (ParseSucc a state)
    StkAlt prevStack _ _ ->
      unwindSucc a state prevStack
    StkTry prevStack _ ->
      unwindSucc a state prevStack
    StkRecover prevStack prevState _ ->
      unwindSucc a (state { consumed = state.consumed || prevState.consumed }) prevStack
    StkLookAhead prevStack prevState ->
      unwindSucc a prevState prevStack
    StkBinds prevStack queue ->
      SuccBinds prevStack state queue
