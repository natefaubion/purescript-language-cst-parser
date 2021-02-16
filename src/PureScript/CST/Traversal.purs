module PureScript.CST.Traversal
  ( Rewrite
  , defaultVisitorM
  , rewriteModuleBottomUpM
  , rewriteBinderBottomUpM
  , rewriteExprBottomUpM
  , rewriteDeclBottomUpM
  , rewriteTypeBottomUpM
  , rewriteModuleTopDownM
  , rewriteBinderTopDownM
  , rewriteExprTopDownM
  , rewriteDeclTopDownM
  , rewriteTypeTopDownM
  , RewriteWithContext
  , defaultVisitorWithContextM
  , rewriteModuleWithContextM
  , rewriteBinderWithContextM
  , rewriteExprWithContextM
  , rewriteDeclWithContextM
  , rewriteTypeWithContextM
  , MonoidalRewrite
  , defaultMonoidalVisitor
  , foldMapModule
  , foldMapBinder
  , foldMapDecl
  , foldMapExpr
  , foldMapType
  , PureRewrite
  , defaultVisitor
  , rewriteModuleBottomUp
  , rewriteBinderBottomUp
  , rewriteExprBottomUp
  , rewriteDeclBottomUp
  , rewriteTypeBottomUp
  , rewriteModuleTopDown
  , rewriteBinderTopDown
  , rewriteExprTopDown
  , rewriteDeclTopDown
  , rewriteTypeTopDown
  , PureRewriteWithContext
  , defaultVisitorWithContext
  , rewriteModuleWithContext
  , rewriteBinderWithContext
  , rewriteExprWithContext
  , rewriteDeclWithContext
  , rewriteTypeWithContext
  ) where

import Prelude

import Control.Monad.Free (Free, runFree)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Data.Bitraversable (bitraverse, ltraverse)
import Data.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), curry, uncurry)
import PureScript.CST.Types (AdoBlock, Binder(..), CaseOf, ClassHead, DataCtor, DataHead, Declaration(..), Delimited, DelimitedNonEmpty, DoBlock, DoStatement(..), Expr(..), Foreign(..), Guarded(..), GuardedExpr, IfThenElse, Instance(..), InstanceBinding(..), InstanceHead, Labeled(..), Lambda, LetBinding(..), LetIn, Module(..), ModuleBody(..), OneOrDelimited(..), PatternGuard, RecordAccessor, RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), ValueBindingFields, Where, Wrapped(..))
import Type.Row (type (+))

type Rewrite e f g = g e -> f (g e)
type RewriteWithContext c e f g = c -> g e -> f (Tuple c (g e))
type MonoidalRewrite e m g = g e -> m
type PureRewrite e g = g e -> g e
type PureRewriteWithContext c e g = c -> g e -> Tuple c (g e)

type OnBinder t r = (onBinder :: t Binder | r)
type OnDecl t r = (onDecl :: t Declaration | r)
type OnExpr t r = (onExpr :: t Expr | r)
type OnType t r = (onType :: t Type | r)

type OnPureScript t =
  ( OnBinder t
  + OnDecl t
  + OnExpr t
  + OnType t
  + ()
  )

defaultVisitorM :: forall e f. Applicative f => { | OnPureScript (Rewrite e f) }
defaultVisitorM =
  { onBinder: pure
  , onDecl: pure
  , onExpr: pure
  , onType: pure
  }

defaultVisitorWithContextM :: forall c e m. Monad m => { | OnPureScript (RewriteWithContext c e m) }
defaultVisitorWithContextM =
  { onBinder: curry pure
  , onDecl: curry pure
  , onExpr: curry pure
  , onType: curry pure
  }

defaultMonoidalVisitor :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) }
defaultMonoidalVisitor =
  { onBinder: mempty
  , onDecl: mempty
  , onExpr: mempty
  , onType: mempty
  }

defaultVisitor :: forall e. { | OnPureScript (PureRewrite e) }
defaultVisitor =
  { onBinder: identity
  , onDecl: identity
  , onExpr: identity
  , onType: identity
  }

defaultVisitorWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) }
defaultVisitorWithContext =
  { onBinder: curry identity
  , onDecl: curry identity
  , onExpr: curry identity
  , onType: curry identity
  }

traverseModule
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnDecl (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Module
traverseModule k (Module mod) = Module <<<
  { header: mod.header
  , body: _
  } <$> traverseModuleBody k mod.body

traverseModuleBody
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnDecl (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f ModuleBody
traverseModuleBody k = Newtype.traverse ModuleBody (\b -> b { decls = _ } <$> traverse (traverseDecl k) b.decls)

traverseDecl
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnDecl (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Declaration
traverseDecl k = case _ of
  DeclData binding ctors -> DeclData <$> traverseDataHead k binding <*> traverse (traverse (traverseSeparated (traverseDataCtor k))) ctors
  DeclType head tok typ -> DeclType <$> traverseDataHead k head <@> tok <*> k.onType typ
  DeclNewtype head tok name typ -> DeclNewtype <$> traverseDataHead k head <@> tok <@> name <*> k.onType typ
  DeclClass head sig -> DeclClass <$> traverseClassHead k head <*> traverse (traverse (traverse (traverseLabeled k.onType))) sig
  DeclInstanceChain instances -> DeclInstanceChain <$> traverseSeparated (traverseInstance k) instances
  DeclDerive tok mbTok head -> DeclDerive tok mbTok <$> traverseInstanceHead k head
  DeclKindSignature tok typ -> DeclKindSignature tok <$> traverseLabeled k.onType typ
  DeclSignature typ -> DeclSignature <$> traverseLabeled k.onType typ
  DeclValue fields -> DeclValue <$> traverseValueBindingFields k fields
  DeclForeign tok1 tok2 f -> DeclForeign tok1 tok2 <$> traverseForeign k f
  decl -> k.onDecl decl

traverseForeign
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f Foreign
traverseForeign k = case _ of
  ForeignValue typ -> ForeignValue <$> traverseLabeled k.onType typ
  ForeignData tok typ -> ForeignData tok <$> traverseLabeled k.onType typ
  ForeignKind tok name -> pure (ForeignKind tok name)

traverseInstance
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Instance
traverseInstance k = Newtype.traverse Instance (\i -> i { head = _, body = _ } <$> traverseInstanceHead k i.head <*> traverse (traverse (traverse (traverseInstanceBinding k))) i.body)

traverseInstanceHead
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f InstanceHead
traverseInstanceHead k head = head { constraints = _, types = _ } <$> traverse (ltraverse (traverseOneOrDelimited k.onType)) head.constraints <*> traverse k.onType head.types

traverseInstanceBinding
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f InstanceBinding
traverseInstanceBinding k = case _ of
  InstanceBindingSignature typ -> InstanceBindingSignature <$> traverseLabeled k.onType typ
  InstanceBindingName fields -> InstanceBindingName <$> traverseValueBindingFields k fields

traverseClassHead
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f ClassHead
traverseClassHead k head = head { super = _, vars = _ } <$> traverse (ltraverse (traverseOneOrDelimited k.onType)) head.super <*> traverse (traverseTypeVarBinding k) head.vars

traverseOneOrDelimited
  :: forall a f
   . Applicative f
  => (a -> f a)
  -> Rewrite a f OneOrDelimited
traverseOneOrDelimited k = case _ of
  One a -> One <$> k a
  Many all -> Many <$> traverseDelimitedNonEmpty k all

traverseDataHead
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f DataHead
traverseDataHead k head = head { vars = _ } <$> traverse (traverseTypeVarBinding k) head.vars

traverseDataCtor
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f DataCtor
traverseDataCtor k ctor = ctor { fields = _ } <$> traverse k.onType ctor.fields

traverseType
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f Type
traverseType k = case _ of
  TypeRow row -> TypeRow <$> traverseWrapped (traverseRow k) row
  TypeRecord row -> TypeRecord <$> traverseWrapped (traverseRow k) row
  TypeForall tok1 bindings tok2 typ -> TypeForall tok1 <$> traverse (traverseTypeVarBinding k) bindings <@> tok2 <*> k.onType typ
  TypeKinded typ1 tok typ2 -> TypeKinded <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeApp typ1 typ2 -> TypeApp <$> k.onType typ1 <*> k.onType typ2
  TypeOp typ1 op typ2 -> TypeOp <$> k.onType typ1 <@> op <*> k.onType typ2
  TypeArr typ1 tok typ2 -> TypeArr <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeConstrained typ1 tok typ2 -> TypeConstrained <$> k.onType typ1 <@> tok <*> k.onType typ2
  TypeParens wrapped -> TypeParens <$> traverseWrapped k.onType wrapped
  TypeUnaryRow tok typ -> TypeUnaryRow tok <$> k.onType typ
  typ -> k.onType typ

traverseRow
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f Row
traverseRow k = Newtype.traverse Row (\r -> r { labels = _, tail = _ } <$> traverse (traverseSeparated (traverseLabeled k.onType)) r.labels <*> traverse (traverse k.onType) r.tail)

traverseTypeVarBinding
  :: forall e f r
   . Applicative f
  => { | OnType (Rewrite e f) + r }
  -> Rewrite e f TypeVarBinding
traverseTypeVarBinding k = case _ of
  TypeVarKinded labeled -> TypeVarKinded <$> traverseWrapped (traverseLabeled k.onType) labeled
  TypeVarName name -> pure (TypeVarName name)

traverseExpr
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Expr
traverseExpr k = case _ of
  ExprArray expr -> ExprArray <$> (traverseDelimited k.onExpr expr)
  ExprRecord expr -> ExprRecord <$> traverseDelimited (traverseRecordLabeled k.onExpr) expr
  ExprParens expr -> ExprParens <$> traverseWrapped k.onExpr expr
  ExprTyped expr tok ty -> ExprTyped <$> k.onExpr expr <@> tok <*> k.onType ty
  ExprInfix expr1 expr2 expr3 -> ExprInfix <$> k.onExpr expr1 <*> traverseWrapped k.onExpr expr2 <*> k.onExpr expr3
  ExprOp expr1 op expr2 -> ExprOp <$> k.onExpr expr1 <@> op <*> k.onExpr expr2
  ExprNegate tok expr -> ExprNegate tok <$> k.onExpr expr
  ExprRecordAccessor recordAccessor -> ExprRecordAccessor <$> traverseRecordAccessor k recordAccessor
  ExprRecordUpdate expr recordUpdates -> ExprRecordUpdate <$> k.onExpr expr <*> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates
  ExprApp expr1 expr2 -> ExprApp <$> k.onExpr expr1 <*> k.onExpr expr2
  ExprLambda lambda -> ExprLambda <$> traverseLambda k lambda
  ExprIf ifThenElse -> ExprIf <$> traverseIfThenElse k ifThenElse
  ExprCase caseOf -> ExprCase <$> traverseCaseOf k caseOf
  ExprLet letIn -> ExprLet <$> traverseLetIn k letIn
  ExprDo doBlock -> ExprDo <$> traverseDoBlock k doBlock
  ExprAdo adoBlock -> ExprAdo <$> traverseAdoBlock k adoBlock
  expr -> k.onExpr expr

traverseDelimited
  :: forall f a
   . Applicative f
  => (a -> f a)
  -> Rewrite a f Delimited
traverseDelimited k = traverseWrapped (traverse (traverseSeparated k))

traverseDelimitedNonEmpty
  :: forall a f
   . Applicative f
  => (a -> f a)
  -> Rewrite a f DelimitedNonEmpty
traverseDelimitedNonEmpty k = traverseWrapped (traverseSeparated k)

traverseSeparated
  :: forall f a
   . Applicative f
  => (a -> f a)
  -> Rewrite a f Separated
traverseSeparated k (Separated sep) = ado
  head <- k sep.head
  tail <- traverse (traverse k) sep.tail
  in Separated { head, tail }

traverseWrapped
 :: forall f a
  . Applicative f
 => (a -> f a)
 -> Rewrite a f Wrapped
traverseWrapped k = Newtype.traverse Wrapped (\w -> w { value = _ } <$> k w.value)

traverseRecordLabeled
  :: forall f a
   . Applicative f
  => (a -> f a)
  -> Rewrite a f RecordLabeled
traverseRecordLabeled k = case _ of
  RecordPun name -> pure (RecordPun name)
  RecordField name tok a -> RecordField name tok <$> k a

traverseLabeled
  :: forall f a b
   . Applicative f
  => (b -> f b)
  -> Rewrite b f (Labeled a)
traverseLabeled k = Newtype.traverse Labeled (\l -> l { value = _ } <$> k l.value)

traverseRecordAccessor
  :: forall e f r
   . Applicative f
  => { | OnExpr (Rewrite e f) + r }
  -> Rewrite e f RecordAccessor
traverseRecordAccessor k r = r { expr = _ } <$> k.onExpr r.expr

traverseRecordUpdate
  :: forall e f r
   . Applicative f
  => { | OnExpr (Rewrite e f) + r }
  -> Rewrite e f RecordUpdate
traverseRecordUpdate k = case _ of
  RecordUpdateLeaf name tok expr -> RecordUpdateLeaf name tok <$> k.onExpr expr
  RecordUpdateBranch name recordUpdates -> RecordUpdateBranch name <$> traverseWrapped (traverseSeparated (traverseRecordUpdate k)) recordUpdates

traverseLambda
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Lambda
traverseLambda k l = l { binders = _, body = _ } <$> traverse (traverseBinder k) l.binders <*> k.onExpr l.body

traverseIfThenElse
  :: forall e f r
   . Applicative f
  => { | OnExpr (Rewrite e f) + r }
  -> Rewrite e f IfThenElse
traverseIfThenElse k r = r { cond = _, true = _, false = _ } <$> k.onExpr r.cond <*> k.onExpr r.true <*> k.onExpr r.false

traverseCaseOf
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f CaseOf
traverseCaseOf k r = r { head = _, branches = _ } <$> traverseSeparated k.onExpr r.head <*> traverse (bitraverse (traverseSeparated (traverseBinder k)) (traverseGuarded k)) r.branches

traverseGuarded
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Guarded
traverseGuarded k = case _ of
  Unconditional tok w -> Unconditional tok <$> traverseWhere k w
  Guarded guards -> Guarded <$> traverse (traverseGuardedExpr k) guards

traverseGuardedExpr
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f GuardedExpr
traverseGuardedExpr k g = g { patterns = _, where = _ } <$> traverseSeparated (traversePatternGuard k) g.patterns <*> traverseWhere k g.where

traversePatternGuard
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f PatternGuard
traversePatternGuard k g = g { binder = _, expr = _ } <$> traverse (ltraverse (traverseBinder k)) g.binder <*> k.onExpr g.expr

traverseWhere
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Where
traverseWhere k w = w { expr = _, bindings = _ } <$> k.onExpr w.expr <*> traverse (traverse (traverse (traverseLetBinding k))) w.bindings

traverseLetBinding
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f LetBinding
traverseLetBinding k = case _ of
  LetBindingSignature name -> LetBindingSignature <$> traverseLabeled k.onType name
  LetBindingName valueBinders -> LetBindingName <$> traverseValueBindingFields k valueBinders
  LetBindingPattern binder tok w -> LetBindingPattern <$> traverseBinder k binder <@> tok <*> traverseWhere k w
  LetBindingError e -> pure (LetBindingError e)

traverseValueBindingFields
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f ValueBindingFields
traverseValueBindingFields k v = v { binders = _, guarded = _ } <$> traverse (traverseBinder k) v.binders <*> traverseGuarded k v.guarded

traverseLetIn
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f LetIn
traverseLetIn k l = l { bindings = _, body = _ } <$> traverse (traverseLetBinding k) l.bindings <*> k.onExpr l.body

traverseDoStatement
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f DoStatement
traverseDoStatement k = case _ of
  DoLet tok letBindings -> DoLet tok <$> traverse (traverseLetBinding k) letBindings
  DoDiscard expr -> DoDiscard <$> k.onExpr expr
  DoBind binder tok expr -> DoBind <$> traverseBinder k binder <@> tok <*> k.onExpr expr
  DoError e -> pure (DoError e)

traverseDoBlock
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f DoBlock
traverseDoBlock k d = d { statements = _ } <$> traverse (traverseDoStatement k) d.statements

traverseAdoBlock
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnExpr (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f AdoBlock
traverseAdoBlock k a = a { statements = _, result = _ } <$> traverse (traverseDoStatement k) a.statements <*> k.onExpr a.result

traverseBinder
  :: forall e f r
   . Applicative f
  => { | OnBinder (Rewrite e f) + OnType (Rewrite e f) + r }
  -> Rewrite e f Binder
traverseBinder k = case _ of
  BinderNamed name tok binder -> BinderNamed name tok <$> traverseBinder k binder
  BinderConstructor name binders -> BinderConstructor name <$> traverse (traverseBinder k) binders
  BinderArray binders -> BinderArray <$> traverseDelimited (traverseBinder k) binders
  BinderRecord binders -> BinderRecord <$> traverseDelimited (traverseRecordLabeled (traverseBinder k)) binders
  BinderParens binder -> BinderParens <$> traverseWrapped (traverseBinder k) binder
  BinderTyped binder tok typ -> BinderTyped <$> traverseBinder k binder <@> tok <*> k.onType typ
  BinderOp binder1 op binder2 -> BinderOp <$> traverseBinder k binder1 <@> op <*> traverseBinder k binder2
  binder -> k.onBinder binder

bottomUpTraversal
  :: forall m e
   . Monad m
  => { | OnPureScript (Rewrite e m) }
  -> { | OnPureScript (Rewrite e m) }
bottomUpTraversal visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> visitor.onBinder =<< traverseBinder visitor' a
    , onExpr:   \a -> visitor.onExpr   =<< traverseExpr visitor' a
    , onType:   \a -> visitor.onType   =<< traverseType visitor' a
    , onDecl:   \a -> visitor.onDecl   =<< traverseDecl visitor' a
    }

rewriteBottomUpM
  :: forall m e g
   . Monad m
  => ({ | OnPureScript (Rewrite e m) } -> Rewrite e m g)
  -> { | OnPureScript (Rewrite e m) }
  -> Rewrite e m g
rewriteBottomUpM traversal visitor = do
  let visitor' = bottomUpTraversal visitor
  traversal visitor'

rewriteModuleBottomUpM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Module
rewriteModuleBottomUpM = rewriteBottomUpM traverseModule

rewriteBinderBottomUpM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Binder
rewriteBinderBottomUpM = rewriteBottomUpM _.onBinder

rewriteExprBottomUpM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Expr
rewriteExprBottomUpM = rewriteBottomUpM _.onExpr

rewriteDeclBottomUpM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Declaration
rewriteDeclBottomUpM = rewriteBottomUpM _.onDecl

rewriteTypeBottomUpM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Type
rewriteTypeBottomUpM = rewriteBottomUpM _.onType

topDownTraversal
  :: forall m e
   . Monad m
  => { | OnPureScript (Rewrite e m) }
  -> { | OnPureScript (Rewrite e m) }
topDownTraversal visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> visitor.onBinder a >>= traverseBinder visitor'
    , onExpr:   \a -> visitor.onExpr a   >>= traverseExpr visitor'
    , onType:   \a -> visitor.onType a   >>= traverseType visitor'
    , onDecl:   \a -> visitor.onDecl a   >>= traverseDecl visitor'
    }

rewriteTopDownM
  :: forall m e g
   . Monad m
  => ({ | OnPureScript (Rewrite e m) } -> Rewrite e m g)
  -> { | OnPureScript (Rewrite e m) }
  -> Rewrite e m g
rewriteTopDownM traversal visitor = do
  let visitor' = topDownTraversal visitor
  traversal visitor'

rewriteModuleTopDownM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Module
rewriteModuleTopDownM = rewriteTopDownM traverseModule

rewriteBinderTopDownM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Binder
rewriteBinderTopDownM = rewriteTopDownM _.onBinder

rewriteDeclTopDownM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Declaration
rewriteDeclTopDownM = rewriteTopDownM _.onDecl

rewriteExprTopDownM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Expr
rewriteExprTopDownM = rewriteTopDownM _.onExpr

rewriteTypeTopDownM :: forall e m. Monad m => { | OnPureScript (Rewrite e m) } -> Rewrite e m Type
rewriteTypeTopDownM = rewriteTopDownM _.onType

topDownTraversalWithContextM
  :: forall c m e
   . Monad m
  => { | OnPureScript (RewriteWithContext c e m) }
  -> { | OnPureScript (Rewrite e (ReaderT c m)) }
topDownTraversalWithContextM visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> ReaderT \ctx -> visitor.onBinder ctx a >>= uncurry (flip (runReaderT <<< traverseBinder visitor'))
    , onExpr: \a -> ReaderT \ctx -> visitor.onExpr ctx a >>= uncurry (flip (runReaderT <<< traverseExpr visitor'))
    , onDecl: \a -> ReaderT \ctx -> visitor.onDecl ctx a >>= uncurry (flip (runReaderT <<< traverseDecl visitor'))
    , onType: \a -> ReaderT \ctx -> visitor.onType ctx a >>= uncurry (flip (runReaderT <<< traverseType visitor'))
    }

rewriteWithContextM
  :: forall c m e g
   . Monad m
  => ({ | OnPureScript (Rewrite e (ReaderT c m)) } -> Rewrite e (ReaderT c m) g)
  -> { | OnPureScript (RewriteWithContext c e m) }
  -> RewriteWithContext c e m g
rewriteWithContextM traversal visitor ctx g = do
  let visitor' = topDownTraversalWithContextM visitor
  Tuple ctx <$> (runReaderT ((traversal visitor') g) ctx)

rewriteModuleWithContextM :: forall c m e. Monad m => { | OnPureScript (RewriteWithContext c e m) } -> RewriteWithContext c e m Module
rewriteModuleWithContextM = rewriteWithContextM traverseModule

rewriteBinderWithContextM :: forall c m e. Monad m => { | OnPureScript (RewriteWithContext c e m) } -> RewriteWithContext c e m Binder
rewriteBinderWithContextM = rewriteWithContextM _.onBinder

rewriteDeclWithContextM :: forall c m e. Monad m => { | OnPureScript (RewriteWithContext c e m) } -> RewriteWithContext c e m Declaration
rewriteDeclWithContextM = rewriteWithContextM _.onDecl

rewriteExprWithContextM :: forall c m e. Monad m => { | OnPureScript (RewriteWithContext c e m) } -> RewriteWithContext c e m Expr
rewriteExprWithContextM = rewriteWithContextM _.onExpr

rewriteTypeWithContextM :: forall c m e. Monad m => { | OnPureScript (RewriteWithContext c e m) } -> RewriteWithContext c e m Type
rewriteTypeWithContextM = rewriteWithContextM _.onType

topDownMonoidalTraversal
  :: forall e m
   . Monoid m
  => { | OnPureScript (MonoidalRewrite e m) }
  -> { | OnPureScript (Rewrite e (Compose (Free Identity) (Const m))) }
topDownMonoidalTraversal visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> Compose (pure (Const (visitor.onBinder a))) <*> traverseBinder visitor' a
    , onExpr: \a -> Compose (pure (Const (visitor.onExpr a))) <*> traverseExpr visitor' a
    , onDecl: \a -> Compose (pure (Const (visitor.onDecl a))) <*> traverseDecl visitor' a
    , onType: \a -> Compose (pure (Const (visitor.onType a))) <*> traverseType visitor' a
    }

monoidalRewrite
  :: forall e m g
   . Monoid m
  => ({ | OnPureScript (Rewrite e (Compose (Free Identity) (Const m))) } -> Rewrite e (Compose (Free Identity) (Const m)) g)
  -> { | OnPureScript (MonoidalRewrite e m) }
  -> MonoidalRewrite e m g
monoidalRewrite traversal visitor g = do
  let visitor' = topDownMonoidalTraversal visitor
  un Const (runFree (un Identity) (un Compose ((traversal visitor') g)))

foldMapModule :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) } -> MonoidalRewrite e m Module
foldMapModule = monoidalRewrite traverseModule

foldMapBinder :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) } -> MonoidalRewrite e m Binder
foldMapBinder = monoidalRewrite _.onBinder

foldMapDecl :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) } -> MonoidalRewrite e m Declaration
foldMapDecl = monoidalRewrite _.onDecl

foldMapExpr :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) } -> MonoidalRewrite e m Expr
foldMapExpr = monoidalRewrite _.onExpr

foldMapType :: forall e m. Monoid m => { | OnPureScript (MonoidalRewrite e m) } -> MonoidalRewrite e m Type
foldMapType = monoidalRewrite _.onType

bottomUpPureTraversal
  :: forall e
   . { | OnPureScript (PureRewrite e) }
  -> { | OnPureScript (Rewrite e (Free Identity)) }
bottomUpPureTraversal visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> pure <<< visitor.onBinder =<< traverseBinder visitor' a
    , onExpr:   \a -> pure <<< visitor.onExpr   =<< traverseExpr visitor' a
    , onType:   \a -> pure <<< visitor.onType   =<< traverseType visitor' a
    , onDecl:   \a -> pure <<< visitor.onDecl   =<< traverseDecl visitor' a
    }

rewriteBottomUp
  :: forall e g
   . ({ | OnPureScript (Rewrite e (Free Identity)) } -> Rewrite e (Free Identity) g)
  -> { | OnPureScript (PureRewrite e) }
  -> PureRewrite e g
rewriteBottomUp traversal visitor = do
  let visitor' = bottomUpPureTraversal visitor
  runFree (un Identity) <<< traversal visitor'

rewriteModuleBottomUp :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Module
rewriteModuleBottomUp = rewriteBottomUp traverseModule

rewriteBinderBottomUp :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Binder
rewriteBinderBottomUp = rewriteBottomUp _.onBinder

rewriteExprBottomUp :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Expr
rewriteExprBottomUp = rewriteBottomUp _.onExpr

rewriteDeclBottomUp :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Declaration
rewriteDeclBottomUp = rewriteBottomUp _.onDecl

rewriteTypeBottomUp :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Type
rewriteTypeBottomUp = rewriteBottomUp _.onType

topDownPureTraversal
  :: forall e
   . { | OnPureScript (PureRewrite e) }
  -> { | OnPureScript (Rewrite e (Free Identity)) }
topDownPureTraversal visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> pure (visitor.onBinder a) >>= traverseBinder visitor'
    , onExpr:   \a -> pure (visitor.onExpr a)   >>= traverseExpr visitor'
    , onType:   \a -> pure (visitor.onType a)   >>= traverseType visitor'
    , onDecl:   \a -> pure (visitor.onDecl a)   >>= traverseDecl visitor'
    }

rewriteTopDown
  :: forall e g
   . ({ | OnPureScript (Rewrite e (Free Identity)) } -> Rewrite e (Free Identity) g)
  -> { | OnPureScript (PureRewrite e) }
  -> PureRewrite e g
rewriteTopDown traversal visitor = do
  let visitor' = topDownPureTraversal visitor
  runFree (un Identity) <<< traversal visitor'

rewriteModuleTopDown :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Module
rewriteModuleTopDown = rewriteTopDown traverseModule

rewriteBinderTopDown :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Binder
rewriteBinderTopDown = rewriteTopDown _.onBinder

rewriteExprTopDown :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Expr
rewriteExprTopDown = rewriteTopDown _.onExpr

rewriteDeclTopDown :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Declaration
rewriteDeclTopDown = rewriteTopDown _.onDecl

rewriteTypeTopDown :: forall e. { | OnPureScript (PureRewrite e) } -> PureRewrite e Type
rewriteTypeTopDown = rewriteTopDown _.onType

topDownTraversalWithContext
  :: forall c e
   . { | OnPureScript (PureRewriteWithContext c e) }
  -> { | OnPureScript (Rewrite e (ReaderT c Identity)) }
topDownTraversalWithContext visitor = visitor'
  where
  visitor' =
    { onBinder: \a -> ReaderT \ctx -> pure (visitor.onBinder ctx a) >>= uncurry (flip (runReaderT <<< traverseBinder visitor'))
    , onExpr: \a -> ReaderT \ctx -> pure (visitor.onExpr ctx a) >>= uncurry (flip (runReaderT <<< traverseExpr visitor'))
    , onDecl: \a -> ReaderT \ctx -> pure (visitor.onDecl ctx a) >>= uncurry (flip (runReaderT <<< traverseDecl visitor'))
    , onType: \a -> ReaderT \ctx -> pure (visitor.onType ctx a) >>= uncurry (flip (runReaderT <<< traverseType visitor'))
    }

rewriteWithContext
  :: forall c e g
   . ({ | OnPureScript (Rewrite e (ReaderT c Identity)) } -> Rewrite e (ReaderT c Identity) g)
  -> { | OnPureScript (PureRewriteWithContext c e) }
  -> PureRewriteWithContext c e g
rewriteWithContext traversal visitor ctx g = do
  let visitor' = topDownTraversalWithContext visitor
  Tuple ctx (un Identity (runReaderT ((traversal visitor') g) ctx))

rewriteModuleWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) } -> PureRewriteWithContext c e Module
rewriteModuleWithContext = rewriteWithContext traverseModule

rewriteBinderWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) } -> PureRewriteWithContext c e Binder
rewriteBinderWithContext = rewriteWithContext _.onBinder

rewriteDeclWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) } -> PureRewriteWithContext c e Declaration
rewriteDeclWithContext = rewriteWithContext _.onDecl

rewriteExprWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) } -> PureRewriteWithContext c e Expr
rewriteExprWithContext = rewriteWithContext _.onExpr

rewriteTypeWithContext :: forall c e. { | OnPureScript (PureRewriteWithContext c e) } -> PureRewriteWithContext c e Type
rewriteTypeWithContext = rewriteWithContext _.onType
