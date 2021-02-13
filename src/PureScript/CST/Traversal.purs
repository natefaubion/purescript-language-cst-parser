module PureScript.CST.Traversal where

import Prelude

import Data.Function (on)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (Expr(..), Separated(..), Wrapped(..), Delimited)

type Traversal a = forall f. Applicative f => (a -> f a) -> a -> f a

traverseExpr1 :: forall a. Traversal (Expr a)
traverseExpr1 k = case _ of
  ExprArray a expr -> ExprArray a <$> delimited expr
  ExprParens a expr -> ExprParens a <$> wrapped (traverseExpr1 k) expr
  ExprTyped a expr tok ty -> ExprTyped a <$> traverseExpr1 k expr <@> tok <@> ty
  ExprInfix a expr1 expr2 expr3 -> ExprInfix a <$> traverseExpr1 k expr1 <*> wrapped (traverseExpr1 k) expr2 <*> traverseExpr1 k expr3
  ExprNegate a tok expr -> ExprNegate a tok <$> traverseExpr1 k expr
  expr -> pure expr
  where
  wrapped f = Newtype.traverse Wrapped (\w -> w { value = _ } <$> f w.value)

  wrapped' f (Wrapped { open, value, close }) = Wrapped <<< { open, close, value: _ } <$> f value

  tuple (Tuple a b) = Tuple a <$> traverseExpr1 k b

  separated (Separated sep) = ado
    head <- traverseExpr1 k sep.head
    tail <- traverse tuple sep.tail
    in Separated { head, tail }

  delimited expr = wrapped' (traverse separated) expr


{-
  ExprHole a (Name Ident)
  ExprSection a SourceToken
  ExprIdent a (QualifiedName Ident)
  ExprConstructor a (QualifiedName Proper)
  ExprBoolean a SourceToken Boolean
  ExprChar a SourceToken Char
  ExprString a SourceToken String
  ExprInt a SourceToken Int
  ExprNumber a SourceToken Number
  ExprArray a (Delimited (Expr a))
  ExprRecord a (Delimited (RecordLabeled (Expr a)))
  ExprParens a (Wrapped (Expr a))
  ExprTyped a (Expr a) SourceToken (Type a)
  ExprInfix a (Expr a) (Wrapped (Expr a)) (Expr a)
  ExprOp a (Expr a) (QualifiedName Operator) (Expr a)
  ExprOpName a (QualifiedName Operator)
  ExprNegate a SourceToken (Expr a)
  ExprRecordAccessor a (RecordAccessor a)
  ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  ExprApp a (Expr a) (Expr a)
  ExprLambda a (Lambda a)
  ExprIf a (IfThenElse a)
  ExprCase a (CaseOf a)
  ExprLet a (LetIn a)
  ExprDo a (DoBlock a)
  ExprAdo a (AdoBlock a)
-}
