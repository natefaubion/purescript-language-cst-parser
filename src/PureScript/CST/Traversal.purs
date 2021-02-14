module PureScript.CST.Traversal where

import Prelude

import Data.Bitraversable (bitraverse, ltraverse)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import PureScript.CST.Types (AdoBlock, Binder, CaseOf, Declaration, Delimited, DelimitedNonEmpty, DoBlock, DoStatement(..), Expr(..), Guarded(..), GuardedExpr, IfThenElse, Labeled(..), Lambda, LetBinding(..), LetIn, PatternGuard, RecordAccessor, RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), ValueBindingFields, Where, Wrapped(..))
import Type.Row (type (+))

--                                         (g ann -> f (g ann)) -> g ann -> f (g ann)
type Traversal a = forall f. Applicative f => (a -> f a) -> a -> f a
type MonadicTraversal a = forall m. Monad m => (a -> m a) -> a -> m a
type MonadicTraversalWithContext a = forall c m. Monad m => (c -> a -> m (Tuple c a)) -> c -> a -> m a
type MonoidalTraversal a = forall m. Monoid m => (a -> m) -> a -> m
type PureTraversal a = (a -> a) -> a -> a
type PureTraversalWithContext a = forall c. (c -> a -> Tuple c a) -> c -> a -> a

type GLanguageTraversal g =
  { onDeclaration :: g Declaration
  , onExpr :: g Expr
  , onBinder :: g Binder
  , onType :: g Type
  }

type BasicTraversal f ann g = g ann -> f (g ann)

type LanguageTraversal f a = GLanguageTraversal (BasicTraversal f a)

type OnDeclaration f a r = (onDeclaration :: BasicTraversal f a Declaration | r)
type OnExpr f a r = (onExpr :: BasicTraversal f a Expr | r)
type OnBinder f a r = (onBinder :: BasicTraversal f a Binder | r)
type OnType f a r = (onType :: BasicTraversal f a Type | r)

traverseType1 :: forall r f a. Applicative f => { | OnType f a + r } -> BasicTraversal f a Type
traverseType1 k = case _ of
  TypeRow a row -> TypeRow a <$> traverseWrapped (traverseRow k) row
  TypeRecord a row -> TypeRecord a <$> traverseWrapped (traverseRow k) row
  TypeForall a tok1 bindings tok2 typ -> TypeForall a tok1 <$> traverse (traverseTypeVarBinding k) bindings <@> tok2 <*> k.onType typ
  TypeKinded a typ1 tok typ2 -> TypeKinded a <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeApp a typ1 typ2 -> TypeApp a <$> k.onType typ1 <*> k.onType typ2
  TypeOp a typ1 op typ2 -> TypeOp a <$> k.onType typ1 <@> op <*> k.onType typ2
  TypeArr a typ1 tok typ2 -> TypeArr a <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeConstrained a typ1 tok typ2 -> TypeConstrained a <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeParens a wrapped -> TypeParens a <$> traverseWrapped k.onType wrapped
  TypeUnaryRow a tok typ -> TypeUnaryRow a tok <$> k.onType typ
  typ -> k.onType typ

traverseRow :: forall f a r. Applicative f => { | OnType f a + r } -> BasicTraversal f a Row
traverseRow k = Newtype.traverse Row (\r -> r { labels = _, tail = _ } <$> traverse (traverseSeparated (traverseLabeled k.onType)) r.labels <*> traverse (traverse k.onType) r.tail)

traverseTypeVarBinding :: forall f a r. Applicative f => { | OnType f a + r } -> BasicTraversal f a TypeVarBinding
traverseTypeVarBinding k = case _ of
  TypeVarKinded labeled -> TypeVarKinded <$> traverseWrapped (traverseLabeled k.onType) labeled
  TypeVarName name -> pure (TypeVarName name)

traverseExpr1 :: forall r f a. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a Expr
traverseExpr1 k = case _ of
  ExprArray a expr -> ExprArray a <$> (traverseDelimited k.onExpr expr)
  ExprRecord a expr -> ExprRecord a <$> traverseDelimited (traverseRecordLabeled k.onExpr) expr
  ExprParens a expr -> ExprParens a <$> traverseWrapped k.onExpr expr
  ExprTyped a expr tok ty -> ExprTyped a <$> k.onExpr expr <@> tok <*> k.onType ty
  ExprInfix a expr1 expr2 expr3 -> ExprInfix a <$> k.onExpr expr1 <*> traverseWrapped k.onExpr expr2 <*> k.onExpr expr3
  ExprOp a expr1 op expr2 -> ExprOp a <$> k.onExpr expr1 <@> op <*> k.onExpr expr2
  ExprNegate a tok expr -> ExprNegate a tok <$> k.onExpr expr
  ExprRecordAccessor a recordAccessor -> ExprRecordAccessor a <$> traverseRecordAccessor k recordAccessor
  ExprRecordUpdate a expr recordUpdates -> ExprRecordUpdate a <$> k.onExpr expr <*> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates
  ExprApp a expr1 expr2 -> ExprApp a <$> k.onExpr expr1 <*> k.onExpr expr2
  ExprLambda a lambda -> ExprLambda a <$> traverseLambda k lambda
  ExprIf a ifThenElse -> ExprIf a <$> traverseIfThenElse k ifThenElse
  ExprCase a caseOf -> ExprCase a <$> traverseCaseOf k caseOf
  ExprLet a letIn -> ExprLet a <$> traverseLetIn k letIn
  ExprDo a doBlock -> ExprDo a <$> traverseDoBlock k doBlock
  ExprAdo a adoBlock -> ExprAdo a <$> traverseAdoBlock k adoBlock
  expr -> k.onExpr expr

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

traverseRecordLabeled :: forall f a. Applicative f => (a -> f a) -> BasicTraversal f a RecordLabeled
traverseRecordLabeled k = case _ of
  RecordPun name -> pure (RecordPun name)
  RecordField name tok a -> RecordField name tok <$> k a

traverseLabeled :: forall f a b. Applicative f => (b -> f b) -> BasicTraversal f b (Labeled a)
traverseLabeled k = Newtype.traverse Labeled (\l -> l { value = _ } <$> k l.value)

traverseRecordAccessor :: forall f a r. Applicative f => { | OnExpr f a + r } -> BasicTraversal f a RecordAccessor
traverseRecordAccessor k r = r { expr = _ } <$> k.onExpr r.expr

traverseRecordUpdate :: forall f a r. Applicative f => { | OnExpr f a + r } -> BasicTraversal f a RecordUpdate
traverseRecordUpdate k = case _ of
  RecordUpdateLeaf name tok expr -> RecordUpdateLeaf name tok <$> k.onExpr expr
  RecordUpdateBranch name recordUpdates -> RecordUpdateBranch name <$> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates

traverseLambda :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + r } -> BasicTraversal f a Lambda
traverseLambda k l = l { binders = _, body = _ } <$> traverse k.onBinder l.binders <*> k.onExpr l.body

traverseIfThenElse :: forall f a r. Applicative f => { | OnExpr f a + r} -> BasicTraversal f a IfThenElse
traverseIfThenElse k r = r { cond = _, true = _, false = _ } <$> k.onExpr r.cond <*> k.onExpr r.true <*> k.onExpr r.false

traverseCaseOf :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a CaseOf
traverseCaseOf k r = r { head = _, branches = _ } <$> traverseSeparated k.onExpr r.head <*> traverse (bitraverse (traverseSeparated k.onBinder) (traverseGuarded k)) r.branches

traverseGuarded :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a Guarded
traverseGuarded k = case _ of
  Unconditional tok w -> Unconditional tok <$> traverseWhere k w
  Guarded guards -> Guarded <$> traverse (traverseGuardedExpr k) guards

traverseGuardedExpr :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a GuardedExpr
traverseGuardedExpr k g = g { patterns = _, where = _ } <$> traverseSeparated (traversePatternGuard k) g.patterns <*> traverseWhere k g.where

traversePatternGuard :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + r } -> BasicTraversal f a PatternGuard
traversePatternGuard k g = g { binder = _, expr = _ } <$> traverse (ltraverse k.onBinder) g.binder <*> k.onExpr g.expr

traverseWhere :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a Where
traverseWhere k w = w { expr = _, bindings = _ } <$> k.onExpr w.expr <*> traverse (traverse (traverse (traverseLetBinding k))) w.bindings

traverseLetBinding :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a LetBinding
traverseLetBinding k = case _ of
  LetBindingSignature a name -> LetBindingSignature a <$> traverseLabeled k.onType name
  LetBindingName a valueBinders -> LetBindingName a <$> traverseValueBindingFields k valueBinders
  LetBindingPattern a binder tok w -> LetBindingPattern a <$> k.onBinder binder <@> tok <*> traverseWhere k w

traverseValueBindingFields :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a ValueBindingFields
traverseValueBindingFields k v = v { binders = _, guarded = _ } <$> traverse k.onBinder v.binders <*> traverseGuarded k v.guarded

traverseLetIn :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a LetIn
traverseLetIn k l = l { bindings = _, body = _ } <$> traverse (traverseLetBinding k) l.bindings <*> k.onExpr l.body

traverseDoStatement :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a DoStatement
traverseDoStatement k = case _ of
  DoLet tok letBindings -> DoLet tok <$> traverse (traverseLetBinding k) letBindings
  DoDiscard expr -> DoDiscard <$> k.onExpr expr
  DoBind binder tok expr -> DoBind <$> k.onBinder binder <@> tok <*> k.onExpr expr

traverseDoBlock :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a DoBlock
traverseDoBlock k d = d { statements = _ } <$> traverse (traverseDoStatement k) d.statements

traverseAdoBlock :: forall f a r. Applicative f => { | OnBinder f a + OnExpr f a + OnType f a + r } -> BasicTraversal f a AdoBlock
traverseAdoBlock k a = a { statements = _, result = _ } <$> traverse (traverseDoStatement k) a.statements <*> k.onExpr a.result















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
