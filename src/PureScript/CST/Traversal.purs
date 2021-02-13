module PureScript.CST.Traversal where

import Prelude

import Data.Bitraversable (bitraverse, ltraverse)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import PureScript.CST.Types (Binder, CaseOf, Declaration, Delimited, DelimitedNonEmpty, DoBlock, DoStatement(..), Expr(..), Guarded(..), GuardedExpr, IfThenElse, Labeled(..), Lambda, LetBinding(..), LetIn, PatternGuard, RecordAccessor, RecordLabeled(..), RecordUpdate(..), Separated(..), Type, ValueBindingFields, Where, Wrapped(..), AdoBlock)
import Type.Row (type (+))

type Traversal a = forall f. Applicative f => (a -> f a) -> a -> f a
type MonadicTraversal a = forall m. Monad m => (a -> m a) -> a -> m a
type MonadicTraversalWithContext a = forall c m. Monad m => (c -> a -> m (Tuple c a)) -> c -> a -> m a
type MonoidalTraversal a = forall m. Monoid m => (a -> m) -> a -> m
type PureTraversal a = (a -> a) -> a -> a
type PureTraversalWithContext a = forall c. (c -> a -> Tuple c a) -> c -> a -> a

type GLanguageTraversal g =
  { declaration :: g Declaration
  , expr :: g Expr
  , binder :: g Binder
  , letBinding :: g LetBinding
  , type :: g Type
  }

type BasicTraversal f ann g = g ann -> f (g ann)
--type TraversalWithContext c f a b g = c -> g a -> f (Tuple c (g b))

type LanguageTraversal f a = GLanguageTraversal (BasicTraversal f a)
--type LanguageTraversalWithContext c f a b = GLanguageTraversal (TraversalWithContext c f a b)

type TraverseDeclaration f a r = (traverseDeclaration :: BasicTraversal f a Declaration | r)
type TraverseExpr f a r = (traverseExpr :: BasicTraversal f a Expr | r)
type TraverseBinder f a r = (traverseBinder :: BasicTraversal f a Binder | r)
type TraverseType f a r = (traverseType :: BasicTraversal f a Type | r)

traverseExpr' :: forall r f a. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a Expr
traverseExpr' k = case _ of
  ExprArray a expr -> ExprArray a <$> (traverseDelimited k.traverseExpr expr)
  ExprRecord a expr -> ExprRecord a <$> traverseDelimited (traverseRecordLabeled k.traverseExpr) expr
  ExprParens a expr -> ExprParens a <$> traverseWrapped k.traverseExpr expr
  ExprTyped a expr tok ty -> ExprTyped a <$> k.traverseExpr expr <@> tok <*> k.traverseType ty
  ExprInfix a expr1 expr2 expr3 -> ExprInfix a <$> k.traverseExpr expr1 <*> traverseWrapped k.traverseExpr expr2 <*> k.traverseExpr expr3
  ExprOp a expr1 op expr2 -> ExprOp a <$> k.traverseExpr expr1 <@> op <*> k.traverseExpr expr2
  ExprNegate a tok expr -> ExprNegate a tok <$> k.traverseExpr expr
  ExprRecordAccessor a recordAccessor -> ExprRecordAccessor a <$> traverseRecordAccessor k recordAccessor
  ExprRecordUpdate a expr recordUpdates -> ExprRecordUpdate a <$> k.traverseExpr expr <*> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates
  ExprApp a expr1 expr2 -> ExprApp a <$> k.traverseExpr expr1 <*> k.traverseExpr expr2
  ExprLambda a lambda -> ExprLambda a <$> traverseLambda k lambda
  ExprIf a ifThenElse -> ExprIf a <$> traverseIfThenElse k ifThenElse
  ExprCase a caseOf -> ExprCase a <$> traverseCaseOf k caseOf
  ExprLet a letIn -> ExprLet a <$> traverseLetIn k letIn
  ExprDo a doBlock -> ExprDo a <$> traverseDoBlock k doBlock
  ExprAdo a adoBlock -> ExprAdo a <$> traverseAdoBlock k adoBlock
  expr -> k.traverseExpr expr

traverseDelimited :: forall f a. Applicative f => (a -> f a) -> BasicTraversal f a Delimited
traverseDelimited k = traverseWrapped (traverse (traverseSeparated k))

traverseDelimitedNonEmpty :: forall a f. Applicative f => (a -> f a) -> BasicTraversal f a DelimitedNonEmpty
traverseDelimitedNonEmpty k = traverseWrapped (traverseSeparated k)

traverseSeparated :: forall f a. Applicative f => (a -> f a) -> BasicTraversal f a Separated
traverseSeparated k (Separated sep) = ado
  head <- k sep.head
  tail <- traverse (traverse k) sep.tail
  in Separated { head, tail }

traverseWrapped :: forall f a. Applicative f => (a -> f a) -> BasicTraversal f a Wrapped
traverseWrapped k = Newtype.traverse Wrapped (\w -> w { value = _ } <$> k w.value)

traverseRecordLabeled :: forall a f. Applicative f => (a -> f a) -> BasicTraversal f a RecordLabeled
traverseRecordLabeled k = case _ of
  RecordPun name -> pure (RecordPun name)
  RecordField name tok a -> RecordField name tok <$> k a

traverseLabeled :: forall a b f. Applicative f => (b -> f b) -> BasicTraversal f b (Labeled a)
traverseLabeled k = Newtype.traverse Labeled (\l -> l { value = _ } <$> k l.value)

traverseRecordAccessor :: forall a f r. Applicative f => { | TraverseExpr f a + r } -> BasicTraversal f a RecordAccessor
traverseRecordAccessor k r = r { expr = _ } <$> k.traverseExpr r.expr

traverseRecordUpdate :: forall a f r. Applicative f => { | TraverseExpr f a + r } -> BasicTraversal f a RecordUpdate
traverseRecordUpdate k = case _ of
  RecordUpdateLeaf name tok expr -> RecordUpdateLeaf name tok <$> k.traverseExpr expr
  RecordUpdateBranch name recordUpdates -> RecordUpdateBranch name <$> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates

traverseLambda :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + r } -> BasicTraversal f a Lambda
traverseLambda k l = l { binders = _, body = _ } <$> traverse k.traverseBinder l.binders <*> k.traverseExpr l.body

traverseIfThenElse :: forall a f r. Applicative f => { | TraverseExpr f a + r} -> BasicTraversal f a IfThenElse
traverseIfThenElse k r = r { cond = _, true = _, false = _ } <$> k.traverseExpr r.cond <*> k.traverseExpr r.true <*> k.traverseExpr r.false

traverseCaseOf :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a CaseOf
traverseCaseOf k r = r { head = _, branches = _ } <$> traverseSeparated k.traverseExpr r.head <*> traverse (bitraverse (traverseSeparated k.traverseBinder) (traverseGuarded k)) r.branches

traverseGuarded :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a Guarded
traverseGuarded k = case _ of
  Unconditional tok w -> Unconditional tok <$> traverseWhere k w
  Guarded guards -> Guarded <$> traverse (traverseGuardedExpr k) guards

traverseGuardedExpr :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a GuardedExpr
traverseGuardedExpr k g = g { patterns = _, where = _ } <$> traverseSeparated (traversePatternGuard k) g.patterns <*> traverseWhere k g.where

traversePatternGuard :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + r } -> BasicTraversal f a PatternGuard
traversePatternGuard k g = g { binder = _, expr = _ } <$> traverse (ltraverse k.traverseBinder) g.binder <*> k.traverseExpr g.expr

traverseWhere :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a Where
traverseWhere k w = w { expr = _, bindings = _ } <$> k.traverseExpr w.expr <*> traverse (traverse (traverse (traverseLetBinding k))) w.bindings

traverseLetBinding :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a LetBinding
traverseLetBinding k = case _ of
  LetBindingSignature a name -> LetBindingSignature a <$> traverseLabeled k.traverseType name
  LetBindingName a valueBinders -> LetBindingName a <$> traverseValueBindingFields k valueBinders
  LetBindingPattern a binder tok w -> LetBindingPattern a <$> k.traverseBinder binder <@> tok <*> traverseWhere k w

traverseValueBindingFields :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a ValueBindingFields
traverseValueBindingFields k v = v { binders = _, guarded = _ } <$> traverse k.traverseBinder v.binders <*> traverseGuarded k v.guarded

traverseLetIn :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a LetIn
traverseLetIn k l = l { bindings = _, body = _ } <$> traverse (traverseLetBinding k) l.bindings <*> k.traverseExpr l.body

traverseDoStatement :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a DoStatement
traverseDoStatement k = case _ of
  DoLet tok letBindings -> DoLet tok <$> traverse (traverseLetBinding k) letBindings
  DoDiscard expr -> DoDiscard <$> k.traverseExpr expr
  DoBind binder tok expr -> DoBind <$> k.traverseBinder binder <@> tok <*> k.traverseExpr expr

traverseDoBlock :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a DoBlock
traverseDoBlock k d = d { statements = _ } <$> traverse (traverseDoStatement k) d.statements

traverseAdoBlock :: forall a f r. Applicative f => { | TraverseBinder f a + TraverseExpr f a + TraverseType f a + r } -> BasicTraversal f a AdoBlock
traverseAdoBlock k a = a { statements = _, result = _ } <$> traverse (traverseDoStatement k) a.statements <*> k.traverseExpr a.result















{-
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

-}
