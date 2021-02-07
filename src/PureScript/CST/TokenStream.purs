module PureScript.CST.TokenStream where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import PureScript.CST.Errors (TokenError)
import PureScript.CST.Types (Comment, LineFeed, SourcePos, SourceToken)

newtype TokenStream = TokenStream (Lazy TokenStep)

derive instance newtypeTokenStream :: Newtype TokenStream _

data TokenStep
  = TokenEOF SourcePos (Array (Comment LineFeed))
  | TokenError SourcePos TokenError (Maybe TokenStream)
  | TokenCons SourceToken SourcePos TokenStream

step :: TokenStream -> TokenStep
step = Lazy.force <<< unwrap

consTokens
  :: forall f
   . Foldable f
  => f SourceToken
  -> Tuple SourcePos TokenStream
  -> Tuple SourcePos TokenStream
consTokens = flip (foldr go)
  where
  go tok (Tuple pos next) =
    Tuple tok.range.start $ TokenStream $ Lazy.defer \_ ->
      TokenCons tok pos next
