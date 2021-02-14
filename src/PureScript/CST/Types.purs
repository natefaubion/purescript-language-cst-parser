module PureScript.CST.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

data Comment l
  = Comment String
  | Space Int
  | Line l

data LineFeed
  = LF
  | CRLF

data SourceStyle
  = ASCII
  | Unicode

derive instance eqSourceTyle :: Eq SourceStyle

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokForall SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokAt
  | TokLowerName (Maybe ModuleName) String
  | TokUpperName (Maybe ModuleName) String
  | TokOperator (Maybe ModuleName) String
  | TokSymbolName (Maybe ModuleName) String
  | TokSymbolArrow SourceStyle
  | TokHole String
  | TokChar String Char
  | TokString String String
  | TokRawString String
  | TokInt String Int
  | TokNumber String Number
  | TokLayoutStart
  | TokLayoutSep
  | TokLayoutEnd

derive instance eqToken :: Eq Token

type SourceToken =
  { range :: SourceRange
  , leadingComments :: Array (Comment LineFeed)
  , trailingComments :: Array (Comment Void)
  , value :: Token
  }

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident

newtype Proper = Proper String

derive newtype instance eqProper :: Eq Proper
derive newtype instance ordProper :: Ord Proper

newtype Label = Label String

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label

newtype Operator = Operator String

derive newtype instance eqOperator :: Eq Operator
derive newtype instance ordOperator :: Ord Operator

newtype Name a = Name
  { token :: SourceToken
  , name :: a
  }

newtype QualifiedName a = QualifiedName
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

newtype Wrapped a = Wrapped
  { open :: SourceToken
  , value :: a
  , close :: SourceToken
  }

derive instance newtypeWrapped :: Newtype (Wrapped a) _

newtype Separated a = Separated
  { head :: a
  , tail :: Array (Tuple SourceToken a)
  }

newtype Labeled a b = Labeled
  { label :: a
  , separator :: SourceToken
  , value  :: b
  }

derive instance newtypeLabeled :: Newtype (Labeled a b) _

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)

data Type a
  = TypeVar a (Name Ident)
  | TypeConstructor a (QualifiedName Proper)
  | TypeWildcard a SourceToken
  | TypeHole a (Name Ident)
  | TypeString a SourceToken String
  | TypeRow a (Wrapped (Row a))
  | TypeRecord a (Wrapped (Row a))
  | TypeForall a SourceToken (NonEmptyArray (TypeVarBinding a)) SourceToken (Type a)
  | TypeKinded a (Type a) SourceToken (Type a)
  | TypeApp a (Type a) (Type a)
  | TypeOp a (Type a) (QualifiedName Operator) (Type a)
  | TypeOpName a (QualifiedName Operator)
  | TypeArr a (Type a) SourceToken (Type a)
  | TypeArrName a SourceToken
  | TypeConstrained a (Type a) SourceToken (Type a)
  | TypeParens a (Wrapped (Type a))
  | TypeUnaryRow a SourceToken (Type a)

data TypeVarBinding a
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (Type a)))
  | TypeVarName (Name Ident)

newtype Row a = Row
  { labels :: Maybe (Separated (Labeled (Name Label) (Type a)))
  , tail :: Maybe (Tuple SourceToken (Type a))
  }

derive instance newtypeRow :: Newtype (Row a) _

data Module a = Module
  { ann :: a
  , keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export a))
  , where :: SourceToken
  , imports :: Array (ImportDecl a)
  , decls :: Array (Declaration a)
  , trailingComments :: Array (Comment LineFeed)
  }

data Export a
  = ExportValue a (Name Ident)
  | ExportOp a (Name Operator)
  | ExportType a (Name Proper) (Maybe (DataMembers a))
  | ExportTypeOp a SourceToken (Name Operator)
  | ExportClass a SourceToken (Name Proper)
  | ExportKind a SourceToken (Name Proper)
  | ExportModule a SourceToken (Name ModuleName)

data DataMembers a
  = DataAll a SourceToken
  | DataEnumerated a (Delimited (Name Proper))

data Declaration a
  = DeclData a (DataHead a) (Maybe (Tuple SourceToken (Separated (DataCtor a))))
  | DeclType a (DataHead a) SourceToken (Type a)
  | DeclNewtype a (DataHead a) SourceToken (Name Proper) (Type a)
  | DeclClass a (ClassHead a) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type a)))))
  | DeclInstanceChain a (Separated (Instance a))
  | DeclDerive a SourceToken (Maybe SourceToken) (InstanceHead a)
  | DeclKindSignature a SourceToken (Labeled (Name Proper) (Type a))
  | DeclSignature a (Labeled (Name Ident) (Type a))
  | DeclValue a (ValueBindingFields a)
  | DeclFixity a FixityFields
  | DeclForeign a SourceToken SourceToken (Foreign a)
  | DeclRole a SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))

newtype Instance a = Instance
  { head :: InstanceHead a
  , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding a)))
  }

derive instance newtypeInstance :: Newtype (Instance a) _

data InstanceBinding a
  = InstanceBindingSignature a (Labeled (Name Ident) (Type a))
  | InstanceBindingName a (ValueBindingFields a)

newtype ImportDecl a = ImportDecl
  { ann :: a
  , keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import a)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

data Import a
  = ImportValue a (Name Ident)
  | ImportOp a (Name Operator)
  | ImportType a (Name Proper) (Maybe (DataMembers a))
  | ImportTypeOp a SourceToken (Name Operator)
  | ImportClass a SourceToken (Name Proper)
  | ImportKind a SourceToken (Name Proper)

type DataHead a =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: Array (TypeVarBinding a)
  }

type DataCtor a =
  { ann :: a
  , name :: Name Proper
  , fields :: Array (Type a)
  }

type ClassHead a =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (OneOrDelimited (Type a)) SourceToken)
  , name :: Name Proper
  , vars :: Array (TypeVarBinding a)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyArray (Name Ident))
  | FundepDetermines (NonEmptyArray (Name Ident)) SourceToken (NonEmptyArray (Name Ident))

type InstanceHead a =
  { keyword :: SourceToken
  , name :: Name Ident
  , separator :: SourceToken
  , constraints :: Maybe (Tuple (OneOrDelimited (Type a)) SourceToken)
  , className :: QualifiedName Proper
  , types :: Array (Type a)
  }

data Fixity
  = Infix
  | Infixl
  | Infixr

data FixityOp
  = FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
  | FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)

type FixityFields =
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }

type ValueBindingFields a =
  { name :: Name Ident
  , binders :: Array (Binder a)
  , guarded :: Guarded a
  }

data Guarded a
  = Unconditional SourceToken (Where a)
  | Guarded (NonEmptyArray (GuardedExpr a))

type GuardedExpr a =
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard a)
  , separator :: SourceToken
  , where :: Where a
  }

type PatternGuard a =
  { binder :: Maybe (Tuple (Binder a) SourceToken)
  , expr :: Expr a
  }

data Foreign a
  = ForeignValue (Labeled (Name Ident) (Type a))
  | ForeignData SourceToken (Labeled (Name Proper) (Type a))
  | ForeignKind SourceToken (Name Proper)

data Role
  = Nominal
  | Representational
  | Phantom

data Expr a
  = ExprHole a (Name Ident)
  | ExprSection a SourceToken
  | ExprIdent a (QualifiedName Ident)
  | ExprConstructor a (QualifiedName Proper)
  | ExprBoolean a SourceToken Boolean
  | ExprChar a SourceToken Char
  | ExprString a SourceToken String
  | ExprInt a SourceToken Int
  | ExprNumber a SourceToken Number
  | ExprArray a (Delimited (Expr a))
  | ExprRecord a (Delimited (RecordLabeled (Expr a)))
  | ExprParens a (Wrapped (Expr a))
  | ExprTyped a (Expr a) SourceToken (Type a)
  | ExprInfix a (Expr a) (Wrapped (Expr a)) (Expr a)
  | ExprOp a (Expr a) (QualifiedName Operator) (Expr a)
  | ExprOpName a (QualifiedName Operator)
  | ExprNegate a SourceToken (Expr a)
  | ExprRecordAccessor a (RecordAccessor a)
  | ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  | ExprApp a (Expr a) (Expr a)
  | ExprLambda a (Lambda a)
  | ExprIf a (IfThenElse a)
  | ExprCase a (CaseOf a)
  | ExprLet a (LetIn a)
  | ExprDo a (DoBlock a)
  | ExprAdo a (AdoBlock a)

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

data RecordUpdate a
  = RecordUpdateLeaf (Name Label) SourceToken (Expr a)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate a))

type RecordAccessor a =
  { expr :: Expr a
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda a =
  { symbol :: SourceToken
  , binders :: NonEmptyArray (Binder a)
  , arrow :: SourceToken
  , body :: Expr a
  }

type IfThenElse a =
  { keyword :: SourceToken
  , cond :: Expr a
  , then :: SourceToken
  , true :: Expr a
  , else :: SourceToken
  , false :: Expr a
  }

type CaseOf a =
  { keyword :: SourceToken
  , head :: Separated (Expr a)
  , of :: SourceToken
  , branches :: NonEmptyArray (Tuple (Separated (Binder a)) (Guarded a))
  }

type LetIn a =
  { keyword :: SourceToken
  , bindings :: NonEmptyArray (LetBinding a)
  , in :: SourceToken
  , body :: Expr a
  }

type Where a =
  { expr :: Expr a
  , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding a)))
  }

data LetBinding a
  = LetBindingSignature a (Labeled (Name Ident) (Type a))
  | LetBindingName a (ValueBindingFields a)
  | LetBindingPattern a (Binder a) SourceToken (Where a)

type DoBlock a =
  { keyword :: SourceToken
  , statements :: NonEmptyArray (DoStatement a)
  }

data DoStatement a
  = DoLet SourceToken (NonEmptyArray (LetBinding a))
  | DoDiscard (Expr a)
  | DoBind (Binder a) SourceToken (Expr a)

type AdoBlock a =
  { keyword :: SourceToken
  , statements :: Array (DoStatement a)
  , in :: SourceToken
  , result :: Expr a
  }

data Binder a
  = BinderWildcard a SourceToken
  | BinderVar a (Name Ident)
  | BinderNamed a (Name Ident) SourceToken (Binder a)
  | BinderConstructor a (QualifiedName Proper) (Array (Binder a))
  | BinderBoolean a SourceToken Boolean
  | BinderChar a SourceToken Char
  | BinderString a SourceToken String
  | BinderInt a (Maybe SourceToken) SourceToken Int
  | BinderNumber a (Maybe SourceToken) SourceToken Number
  | BinderArray a (Delimited (Binder a))
  | BinderRecord a (Delimited (RecordLabeled (Binder a)))
  | BinderParens a (Wrapped (Binder a))
  | BinderTyped a (Binder a) SourceToken (Type a)
  | BinderOp a (Binder a) (QualifiedName Operator) (Binder a)
