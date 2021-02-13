module PureScript.CST.Traversal
  ( rewriteExprBottomUpM
  , rewriteExprTopDownM
  , rewriteExprWithContextM
  , rewriteExprBottomUp
  , rewriteExprTopDown
  , rewriteExprWithContext
  , foldMapExpr
  ) where

import Prelude

import Control.Monad.Free (runFree)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Data.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple (Tuple, uncurry)
import PureScript.CST.Types (CaseOf, Delimited, DelimitedNonEmpty, DoBlock, DoStatement(..), Expr(..), Guarded(..), GuardedExpr, IfThenElse, Lambda, LetBinding(..), LetIn, PatternGuard, RecordAccessor, RecordLabeled(..), RecordUpdate(..), Separated(..), ValueBindingFields, Where, Wrapped(..), AdoBlock)

type Traversal a = forall f. Applicative f => (a -> f a) -> a -> f a
type MonadicTraversal a = forall m. Monad m => (a -> m a) -> a -> m a
type MonadicTraversalWithContext a = forall c m. Monad m => (c -> a -> m (Tuple c a)) -> c -> a -> m a
type MonoidalTraversal a = forall m. Monoid m => (a -> m) -> a -> m
type PureTraversal a = (a -> a) -> a -> a
type PureTraversalWithContext a = forall c. (c -> a -> Tuple c a) -> c -> a -> a

bottomUpTraversal :: Traversal ~> MonadicTraversal
bottomUpTraversal traversal k = go
  where go a = k =<< traversal go a

topDownTraversal :: Traversal ~> MonadicTraversal
topDownTraversal traversal k = go
  where go a = k a >>= traversal go

topDownTraversalWithContext :: Traversal ~> MonadicTraversalWithContext
topDownTraversalWithContext traversal k = flip (runReaderT <<< go)
  where go a = ReaderT \ctx -> k ctx a >>= uncurry (flip (runReaderT <<< traversal go))

monoidalTraversal :: Traversal ~> MonoidalTraversal
monoidalTraversal traversal k = un Const <<< runFree (un Identity) <<< un Compose <<< go
  where go a = Compose (pure (Const (k a))) <*> traversal go a

purely :: MonadicTraversal ~> PureTraversal
purely traversal k = runFree (un Identity) <<< traversal (pure <<< k)

purelyWithContext :: MonadicTraversalWithContext ~> PureTraversalWithContext
purelyWithContext traversal k c = runFree (un Identity) <<< traversal (\c' a' -> pure (k c' a')) c

rewriteExprBottomUpM :: forall a. MonadicTraversal (Expr a)
rewriteExprBottomUpM = bottomUpTraversal traverseExpr1

rewriteExprTopDownM :: forall a. MonadicTraversal (Expr a)
rewriteExprTopDownM = topDownTraversal traverseExpr1

rewriteExprWithContextM :: forall a. MonadicTraversalWithContext (Expr a)
rewriteExprWithContextM = topDownTraversalWithContext traverseExpr1

rewriteExprBottomUp :: forall a. PureTraversal (Expr a)
rewriteExprBottomUp = purely rewriteExprBottomUpM

rewriteExprTopDown :: forall a. PureTraversal (Expr a)
rewriteExprTopDown = purely rewriteExprTopDownM

rewriteExprWithContext :: forall a. PureTraversalWithContext (Expr a)
rewriteExprWithContext = purelyWithContext rewriteExprWithContextM

foldMapExpr :: forall a. MonoidalTraversal (Expr a)
foldMapExpr = monoidalTraversal traverseExpr1

traverseExpr1 :: forall a. Traversal (Expr a)
traverseExpr1 k = case _ of
  ExprArray a expr -> ExprArray a <$> traverseDelimited k expr
  ExprRecord a expr -> ExprRecord a <$> traverseDelimited (traverseRecordLabeled k) expr
  ExprParens a expr -> ExprParens a <$> traverseWrapped k expr
  ExprTyped a expr tok ty -> ExprTyped a <$> k expr <@> tok <@> ty
  ExprInfix a expr1 expr2 expr3 -> ExprInfix a <$> k expr1 <*> traverseWrapped k expr2 <*> k expr3
  ExprOp a expr1 op expr2 -> ExprOp a <$> k expr1 <@> op <*> k expr2
  ExprNegate a tok expr -> ExprNegate a tok <$> k expr
  ExprRecordAccessor a recordAccessor -> ExprRecordAccessor a <$> traverseRecordAccessor k recordAccessor
  ExprRecordUpdate a expr recordUpdates -> ExprRecordUpdate a <$> k expr <*> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates
  ExprApp a expr1 expr2 -> ExprApp a <$> k expr1 <*> k expr2
  ExprLambda a lambda -> ExprLambda a <$> traverseLambda k lambda
  ExprIf a ifThenElse -> ExprIf a <$> traverseIfThenElse k ifThenElse
  ExprCase a caseOf -> ExprCase a <$> traverseCaseOf k caseOf
  ExprLet a letIn -> ExprLet a <$> traverseLetIn k letIn
  ExprDo a doBlock -> ExprDo a <$> traverseDoBlock k doBlock
  ExprAdo a adoBlock -> ExprAdo a <$> traverseAdoBlock k adoBlock
  expr -> k expr

traverseAdoBlock :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> AdoBlock a -> f (AdoBlock a)
traverseAdoBlock k a = a { statements = _, result = _ } <$> traverse (traverseDoStatement k) a.statements <*> k a.result

traverseDoBlock :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> DoBlock a -> f (DoBlock a)
traverseDoBlock k d = d { statements = _ } <$> traverse (traverseDoStatement k) d.statements

traverseDoStatement :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> DoStatement a -> f (DoStatement a)
traverseDoStatement k = case _ of
  DoLet tok letBindings -> DoLet tok <$> traverse (traverseLetBinding k) letBindings
  DoDiscard expr -> DoDiscard <$> k expr
  DoBind binder tok expr -> DoBind binder tok <$> k expr

traverseLetIn :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> LetIn a -> f (LetIn a)
traverseLetIn k l = l { bindings = _, body = _ } <$> traverse (traverseLetBinding k) l.bindings <*> k l.body

traverseCaseOf :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> CaseOf a -> f (CaseOf a)
traverseCaseOf k r = r { head = _, branches = _ } <$> traverseSeparated k r.head <*> traverse (traverse (traverseGuarded k)) r.branches

traverseGuarded :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> Guarded a -> f (Guarded a)
traverseGuarded k = case _ of
  Unconditional tok w -> Unconditional tok <$> traverseWhere k w
  Guarded guards -> Guarded <$> traverse (traverseGuardedExpr k) guards

traverseGuardedExpr :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> GuardedExpr a -> f (GuardedExpr a)
traverseGuardedExpr k g = g { patterns = _, where = _ } <$> traverseSeparated (traversePatternGuard k) g.patterns <*> traverseWhere k g.where

traversePatternGuard :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> PatternGuard a -> f (PatternGuard a)
traversePatternGuard k g = g { expr = _ } <$> k g.expr

traverseWhere :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> Where a -> f (Where a)
traverseWhere k w = w { expr = _, bindings = _ } <$> k w.expr <*> traverse (traverse (traverse (traverseLetBinding k))) w.bindings

traverseLetBinding :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> LetBinding a -> f (LetBinding a)
traverseLetBinding k = case _ of
  LetBindingSignature a name -> pure (LetBindingSignature a name)
  LetBindingName a valueBinders -> LetBindingName a <$> traverseValueBindingFields k valueBinders
  LetBindingPattern a binder tok w -> LetBindingPattern a binder tok <$> traverseWhere k w

traverseValueBindingFields :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> ValueBindingFields a -> f (ValueBindingFields a)
traverseValueBindingFields k v = v { guarded = _ } <$> traverseGuarded k v.guarded

traverseIfThenElse :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> IfThenElse a -> f (IfThenElse a)
traverseIfThenElse k r = r { cond = _, true = _, false = _ } <$> k r.cond <*> k r.true <*> k r.false

traverseLambda :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> Lambda a -> f (Lambda a)
traverseLambda k l = l { body = _ } <$> k l.body

traverseRecordUpdate :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> RecordUpdate a -> f (RecordUpdate a)
traverseRecordUpdate k = case _ of
  RecordUpdateLeaf name tok expr -> RecordUpdateLeaf name tok <$> k expr
  RecordUpdateBranch name recordUpdates -> RecordUpdateBranch name <$> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates

traverseRecordAccessor :: forall a f. Applicative f => (Expr a -> f (Expr a)) -> RecordAccessor a -> f (RecordAccessor a)
traverseRecordAccessor k r = r { expr = _ } <$> k r.expr

traverseSeparated :: forall a f. Applicative f => (a -> f a) -> Separated a -> f (Separated a)
traverseSeparated k (Separated sep) = ado
  head <- k sep.head
  tail <- traverse (traverse k) sep.tail
  in Separated { head, tail }

traverseDelimited :: forall a f. Applicative f => (a -> f a) -> Delimited a -> f (Delimited a)
traverseDelimited k = traverseWrapped (traverse (traverseSeparated k))

traverseWrapped :: forall a f. Applicative f => (a -> f a) -> Wrapped a -> f (Wrapped a)
traverseWrapped k = Newtype.traverse Wrapped (\w -> w { value = _ } <$> k w.value)

traverseRecordLabeled :: forall a f. Applicative f => (a -> f a) -> RecordLabeled a -> f (RecordLabeled a)
traverseRecordLabeled k = case _ of
  RecordPun name -> pure (RecordPun name)
  RecordField name tok a -> RecordField name tok <$> k a

traverseDelimitedNonEmpty :: forall a f. Applicative f => (a -> f a) -> DelimitedNonEmpty a -> f (DelimitedNonEmpty a)
traverseDelimitedNonEmpty k = traverseWrapped (traverseSeparated k)
