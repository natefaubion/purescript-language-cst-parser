module PureScript.CST.TokenStream
  ( TokenStream(..)
  , TokenStep(..)
  , step
  , consTokens
  , layoutStack
  , unwindLayout
  , currentIndentColumn
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import PureScript.CST.Errors (ParseError)
import PureScript.CST.Layout (LayoutDelim(..), LayoutStack, currentIndent, isIndented, lytToken)
import PureScript.CST.Types (Comment, LineFeed, SourcePos, SourceToken, Token(..))

newtype TokenStream = TokenStream (Lazy TokenStep)

derive instance newtypeTokenStream :: Newtype TokenStream _

data TokenStep
  = TokenEOF SourcePos (Array (Comment LineFeed))
  | TokenError SourcePos ParseError (Maybe TokenStream) LayoutStack
  | TokenCons SourceToken SourcePos TokenStream LayoutStack

step :: TokenStream -> TokenStep
step = Lazy.force <<< unwrap

consTokens
  :: forall f
   . Foldable f
  => f (Tuple SourceToken LayoutStack)
  -> Tuple SourcePos TokenStream
  -> Tuple SourcePos TokenStream
consTokens = flip (foldr go)
  where
  go (Tuple tok stk) (Tuple pos next) =
    Tuple tok.range.start $ TokenStream $ Lazy.defer \_ ->
      TokenCons tok pos next stk

layoutStack :: TokenStream -> LayoutStack
layoutStack stream = case step stream of
  TokenEOF _ _ -> Nil
  TokenError _ _ _ stk -> stk
  TokenCons _ _ _ stk -> stk

unwindLayout :: SourcePos -> TokenStream -> LayoutStack -> TokenStream
unwindLayout pos eof = go
  where
  go stk = TokenStream $ Lazy.defer \_ -> case stk of
    Nil -> step eof
    Tuple pos' lyt : tl ->
      case lyt of
        LytRoot ->
          step eof
        _
          | isIndented lyt ->
              TokenCons (lytToken pos (TokLayoutEnd pos'.column)) pos (go tl) tl
          | otherwise ->
              step (go tl)

-- In the token stream, the layout stack represents the state after the token.
-- When determining the current indent level, this creates an edge case relating
-- to TokLayoutEnd. The layout stack will return the next indent, but for the
-- purposes of recovery, we want TokLayoutEnd column to be included as the current
-- indent, necessitating special handling.
currentIndentColumn :: TokenStream -> Int
currentIndentColumn stream = case step stream of
  TokenError _ _ _ stk ->
    stkColumn stk
  TokenEOF _ _ ->
    0
  TokenCons { value: TokLayoutEnd col } _ _ _ ->
    col
  TokenCons _ _ _ stk ->
    stkColumn stk
  where
  stkColumn = maybe 0 _.column <<< currentIndent
