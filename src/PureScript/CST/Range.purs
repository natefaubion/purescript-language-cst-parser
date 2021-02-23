module PureScript.CST.Range
  ( class RangeOf
  , rangeOf
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import PureScript.CST.Errors (RecoveredError(..))
import PureScript.CST.Types (Binder(..), ClassFundep(..), DataMembers(..), Declaration(..), DoStatement(..), Export(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), Labeled(..), LetBinding(..), Module(..), ModuleBody(..), ModuleHeader(..), Name(..), OneOrDelimited(..), QualifiedName(..), Separated(..), SourceRange, Type(..), TypeVarBinding(..), Where(..), Wrapped(..))

class RangeOf a where
  rangeOf :: a -> SourceRange

instance rangeOfVoid :: RangeOf Void where
  rangeOf = absurd

instance rangeOfRecoveredError :: RangeOf RecoveredError where
  rangeOf (RecoveredError { position, tokens }) =
    case NonEmptyArray.fromArray tokens of
      Just toks ->
        { start: (NonEmptyArray.head toks).range.start
        , end: (NonEmptyArray.last toks).range.end
        }
      Nothing ->
        { start: position
        , end: position
        }

instance rangeOfModule :: RangeOf (Module e) where
  rangeOf (Module { header: ModuleHeader { keyword }, body: ModuleBody { end } }) =
    { start: keyword.range.start
    , end
    }

instance rangeOfName :: RangeOf (Name a) where
  rangeOf (Name { token }) = token.range

instance rangeOfQualifiedName :: RangeOf (QualifiedName a) where
  rangeOf (QualifiedName { token }) = token.range

instance rangeOfWrapped :: RangeOf (Wrapped a) where
  rangeOf (Wrapped { open, close }) =
    { start: open.range.start
    , end: close.range.end
    }

instance rangeOfSeparated :: RangeOf a => RangeOf (Separated a) where
  rangeOf (Separated { head, tail }) =
    case Array.last tail of
      Just (Tuple _ last) ->
        { start: (rangeOf head).start
        , end: (rangeOf last).end
        }
      Nothing ->
        rangeOf head

instance rangeOfLabeled :: (RangeOf a, RangeOf b) => RangeOf (Labeled a b) where
  rangeOf (Labeled { label, value }) =
    { start: (rangeOf label).start
    , end: (rangeOf label).end
    }

instance rangeOfOneOrDelimited :: RangeOf a => RangeOf (OneOrDelimited a) where
  rangeOf = case _ of
    One a -> rangeOf a
    Many as -> rangeOf as

instance rangeOfType :: RangeOf e => RangeOf (Type e) where
  rangeOf = case _ of
    TypeVar n ->
      rangeOf n
    TypeConstructor n ->
      rangeOf n
    TypeWildcard t ->
      t.range
    TypeHole n ->
      rangeOf n
    TypeString t _ ->
      t.range
    TypeRow w ->
      rangeOf w
    TypeRecord w ->
      rangeOf w
    TypeForall t _ _ ty ->
      { start: t.range.start
      , end: (rangeOf ty).end
      }
    TypeKinded ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeApp ty tys ->
      { start: (rangeOf ty).start
      , end: (rangeOf (NonEmptyArray.last tys)).end
      }
    TypeOp ty ops ->
      { start: (rangeOf ty).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    TypeOpName n ->
      rangeOf n
    TypeArr ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeArrName t ->
      t.range
    TypeConstrained ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeParens w ->
      rangeOf w
    TypeUnaryRow t ty ->
      { start: t.range.start
      , end: (rangeOf ty).end
      }
    TypeError e ->
      rangeOf e

instance rangeOfTypeVarBinding :: RangeOf (TypeVarBinding e) where
  rangeOf = case _ of
    TypeVarKinded w ->
      rangeOf w
    TypeVarName n ->
      rangeOf n

instance rangeOfExport :: RangeOf e => RangeOf (Export e) where
  rangeOf = case _ of
    ExportValue n ->
      rangeOf n
    ExportOp n ->
      rangeOf n
    ExportType n dms ->
      case dms of
        Nothing ->
          rangeOf n
        Just dms' ->
          { start: (rangeOf n).start
          , end: (rangeOf dms').end
          }
    ExportTypeOp t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportClass t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportKind t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportModule t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportError e ->
      rangeOf e

instance rangeOfDataMembers :: RangeOf DataMembers where
  rangeOf = case _ of
    DataAll t ->
      t.range
    DataEnumerated w ->
      rangeOf w

instance rangeOfImportDecl :: RangeOf (ImportDecl e) where
  rangeOf (ImportDecl { keyword, "module": mod, names, qualified }) = do
    let
      { end } = case qualified of
        Nothing ->
          case names of
            Nothing ->
              rangeOf mod
            Just (Tuple _ imports) ->
              rangeOf imports
        Just (Tuple _ n) ->
          rangeOf n
    { start: keyword.range.start
    , end
    }

instance rangeOfImport :: RangeOf e => RangeOf (Import e) where
  rangeOf = case _ of
    ImportValue n ->
      rangeOf n
    ImportOp n ->
      rangeOf n
    ImportType n dms ->
      case dms of
        Nothing ->
          rangeOf n
        Just dms' ->
          { start: (rangeOf n).start
          , end: (rangeOf dms').end
          }
    ImportTypeOp t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ImportClass t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ImportKind t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ImportError e ->
      rangeOf e

instance rangeOfDecl :: RangeOf e => RangeOf (Declaration e) where
  rangeOf = case _ of
    DeclData { keyword, name, vars } ctors -> do
      let
        { end } = case ctors of
          Nothing ->
            case Array.last vars of
              Nothing ->
                rangeOf name
              Just var ->
                rangeOf var
          Just (Tuple _ (Separated { head, tail })) -> do
            let
              { name, fields } =
                maybe head snd $ Array.last tail
            case Array.last fields of
              Nothing ->
                rangeOf name
              Just ty ->
                rangeOf ty
      { start: keyword.range.start
      , end
      }
    DeclType { keyword } _ ty ->
      { start: keyword.range.start
      , end: (rangeOf ty).end
      }
    DeclNewtype { keyword } _ _ ty ->
      { start: keyword.range.start
      , end: (rangeOf ty).end
      }
    DeclClass { keyword, name, vars, fundeps } members -> do
      let
        { end } = case members of
          Nothing ->
            case fundeps of
              Nothing ->
                case Array.last vars of
                  Nothing ->
                    rangeOf name
                  Just var ->
                    rangeOf var
              Just (Tuple _ fundeps) ->
                rangeOf fundeps
          Just (Tuple _ ms) ->
            rangeOf (NonEmptyArray.last ms)
      { start: keyword.range.start
      , end
      }
    DeclInstanceChain insts ->
      rangeOf insts
    DeclDerive keyword _ { className, types } -> do
      let
        { end } = case Array.last types of
          Nothing ->
            rangeOf className
          Just ty ->
            rangeOf ty
      { start: keyword.range.start
      , end
      }
    DeclKindSignature keyword lbl ->
      { start: keyword.range.start
      , end: (rangeOf lbl).end
      }
    DeclSignature sig ->
      rangeOf sig
    DeclValue { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }
    DeclFixity { keyword: Tuple keyword _, operator } ->
      { start: keyword.range.start
      , end: (rangeOf operator).end
      }
    DeclForeign keyword _ frn ->
      { start: keyword.range.start
      , end: (rangeOf frn).end
      }
    DeclRole keyword _ _ roles ->
      { start: keyword.range.start
      , end: (fst (NonEmptyArray.last roles)).range.end
      }
    DeclError e ->
      rangeOf e

instance rangeOfClassFundep :: RangeOf ClassFundep where
  rangeOf = case _ of
    FundepDetermined t ns ->
      { start: t.range.start
      , end: (rangeOf (NonEmptyArray.last ns)).end
      }
    FundepDetermines ns1 _ ns2 ->
      { start: (rangeOf (NonEmptyArray.head ns1)).start
      , end: (rangeOf (NonEmptyArray.last ns2)).end
      }

instance rangeOfInstance :: RangeOf e => RangeOf (Instance e) where
  rangeOf (Instance { head: { keyword, className, types }, body }) = do
    let
      { end } = case body of
        Nothing ->
          case Array.last types of
            Nothing ->
              rangeOf className
            Just ty ->
              rangeOf ty
        Just (Tuple _ bs) ->
          rangeOf (NonEmptyArray.last bs)
    { start: keyword.range.start
    , end
    }

instance rangeOfGuarded :: RangeOf e => RangeOf (Guarded e) where
  rangeOf = case _ of
    Unconditional t wh ->
      { start: t.range.start
      , end: (rangeOf wh).end
      }
    Guarded gs ->
      { start: (NonEmptyArray.head gs).bar.range.start
      , end: (rangeOf (NonEmptyArray.last gs).where).end
      }

instance rangeOfFixityOp :: RangeOf FixityOp where
  rangeOf = case _ of
    FixityValue n1 _ n2 ->
      { start: (rangeOf n1).start
      , end: (rangeOf n2).end
      }
    FixityType t _ _ n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }

instance rangeOfForeign :: RangeOf e => RangeOf (Foreign e) where
  rangeOf = case _ of
    ForeignValue lbl ->
      rangeOf lbl
    ForeignData t lbl ->
      { start: t.range.start
      , end: (rangeOf lbl).end
      }
    ForeignKind t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }

instance rangeOfInstanceBinding :: RangeOf e => RangeOf (InstanceBinding e) where
  rangeOf = case _ of
    InstanceBindingSignature lbl ->
      rangeOf lbl
    InstanceBindingName { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }

instance rangeOfExpr :: RangeOf e => RangeOf (Expr e) where
  rangeOf = case _ of
    ExprHole n ->
      rangeOf n
    ExprSection t ->
      t.range
    ExprIdent n ->
      rangeOf n
    ExprConstructor n ->
      rangeOf n
    ExprBoolean t _ ->
      t.range
    ExprChar t _ ->
      t.range
    ExprString t _ ->
      t.range
    ExprInt t _ ->
      t.range
    ExprNumber t _ ->
      t.range
    ExprArray exprs ->
      rangeOf exprs
    ExprRecord exprs ->
      rangeOf exprs
    ExprParens w ->
      rangeOf w
    ExprTyped expr _ ty ->
      { start: (rangeOf expr).start
      , end: (rangeOf ty).end
      }
    ExprInfix expr ops ->
      { start: (rangeOf expr).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    ExprOp expr ops ->
      { start: (rangeOf expr).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    ExprOpName n ->
      rangeOf n
    ExprNegate t expr ->
      { start: t.range.start
      , end: (rangeOf expr).end
      }
    ExprRecordAccessor { expr, path } ->
      { start: (rangeOf expr).start
      , end: (rangeOf path).end
      }
    ExprRecordUpdate expr upds ->
      { start: (rangeOf expr).start
      , end: (rangeOf upds).end
      }
    ExprApp expr exprs ->
      { start: (rangeOf expr).start
      , end: (rangeOf (NonEmptyArray.last exprs)).end
      }
    ExprLambda { symbol, body } ->
      { start: symbol.range.start
      , end: (rangeOf body).end
      }
    ExprIf ifte ->
      { start: ifte.keyword.range.start
      , end: (rangeOf ifte.false).end
      }
    ExprCase { keyword, branches } ->
      { start: keyword.range.start
      , end: (rangeOf (snd (NonEmptyArray.last branches))).end
      }
    ExprLet { keyword, body } ->
      { start: keyword.range.start
      , end: (rangeOf body).end
      }
    ExprDo { keyword, statements } ->
      { start: keyword.range.start
      , end: (rangeOf (NonEmptyArray.last statements)).end
      }
    ExprAdo { keyword, result } ->
      { start: keyword.range.start
      , end: (rangeOf result).end
      }
    ExprError e ->
      rangeOf e

instance rangeOfDoStatement :: RangeOf e => RangeOf (DoStatement e) where
  rangeOf = case _ of
    DoLet t bindings ->
      { start: t.range.start
      , end: (rangeOf (NonEmptyArray.last bindings)).end
      }
    DoDiscard expr ->
      rangeOf expr
    DoBind b _ expr ->
      { start: (rangeOf b).start
      , end: (rangeOf expr).end
      }
    DoError e ->
      rangeOf e

instance rangeOfLetBinding :: RangeOf e => RangeOf (LetBinding e) where
  rangeOf = case _ of
    LetBindingSignature lbl ->
      rangeOf lbl
    LetBindingName { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }
    LetBindingPattern b _ wh ->
      { start: (rangeOf b).start
      , end: (rangeOf wh).end
      }
    LetBindingError e ->
      rangeOf e

instance rangeOfBinder :: RangeOf e => RangeOf (Binder e) where
  rangeOf = case _ of
    BinderWildcard t ->
      t.range
    BinderVar n ->
      rangeOf n
    BinderNamed n _ b ->
      { start: (rangeOf n).start
      , end: (rangeOf b).end
      }
    BinderConstructor n bs ->
      case Array.last bs of
        Nothing ->
          rangeOf n
        Just b ->
          { start: (rangeOf n).start
          , end: (rangeOf b).end
          }
    BinderBoolean t _ ->
      t.range
    BinderChar t _ ->
      t.range
    BinderString t _ ->
      t.range
    BinderInt neg t _ ->
      case neg of
        Nothing ->
          t.range
        Just n ->
          { start: n.range.start
          , end: t.range.end
          }
    BinderNumber neg t _ ->
      case neg of
        Nothing ->
          t.range
        Just n ->
          { start: n.range.start
          , end: t.range.end
          }
    BinderArray bs ->
      rangeOf bs
    BinderRecord bs ->
      rangeOf bs
    BinderParens b ->
      rangeOf b
    BinderTyped b _ ty ->
      { start: (rangeOf b).start
      , end: (rangeOf ty).end
      }
    BinderOp b ops ->
      { start: (rangeOf b).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    BinderError e ->
      rangeOf e

instance rangeOfWhere :: RangeOf e => RangeOf (Where e) where
  rangeOf (Where { expr, bindings }) = case bindings of
    Nothing ->
      rangeOf expr
    Just (Tuple _ lb) ->
      { start: (rangeOf expr).start
      , end: (rangeOf (NonEmptyArray.last lb)).end
      }
