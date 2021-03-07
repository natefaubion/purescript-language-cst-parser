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
  | Line l Int

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
  | TokLayoutStart Int
  | TokLayoutSep Int
  | TokLayoutEnd Int

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
derive instance newtypeIdent :: Newtype Ident _

newtype Proper = Proper String

derive newtype instance eqProper :: Eq Proper
derive newtype instance ordProper :: Ord Proper
derive instance newtypeProper :: Newtype Proper _

newtype Label = Label String

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label
derive instance newtypeLabel :: Newtype Label _

newtype Operator = Operator String

derive newtype instance eqOperator :: Eq Operator
derive newtype instance ordOperator :: Ord Operator
derive instance newtypeOperator :: Newtype Operator _

newtype Name a = Name
  { token :: SourceToken
  , name :: a
  }

derive instance newtypeName :: Newtype (Name a) _

newtype QualifiedName a = QualifiedName
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _

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

derive instance newtypeSeparated :: Newtype (Separated a) _

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

data PSType e
  = TypeVar (Name Ident)
  | TypeConstructor (QualifiedName Proper)
  | TypeWildcard SourceToken
  | TypeHole (Name Ident)
  | TypeString SourceToken String
  | TypeRow (Wrapped (PSRow e))
  | TypeRecord (Wrapped (PSRow e))
  | TypeForall SourceToken (NonEmptyArray (TypeVarBinding e)) SourceToken (PSType e)
  | TypeKinded (PSType e) SourceToken (PSType e)
  | TypeApp (PSType e) (NonEmptyArray (PSType e))
  | TypeOp (PSType e) (NonEmptyArray (Tuple (QualifiedName Operator) (PSType e)))
  | TypeOpName (QualifiedName Operator)
  | TypeArr (PSType e) SourceToken (PSType e)
  | TypeArrName SourceToken
  | TypeConstrained (PSType e) SourceToken (PSType e)
  | TypeParens (Wrapped (PSType e))
  | TypeUnaryRow SourceToken (PSType e)
  | TypeError e

data TypeVarBinding e
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (PSType e)))
  | TypeVarName (Name Ident)

newtype PSRow e = PSRow
  { labels :: Maybe (Separated (Labeled (Name Label) (PSType e)))
  , tail :: Maybe (Tuple SourceToken (PSType e))
  }

derive instance newtypeRow :: Newtype (PSRow e) _

newtype PSModule e = PSModule
  { header :: ModuleHeader e
  , body :: ModuleBody e
  }

newtype ModuleHeader e = ModuleHeader
  { keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export e))
  , where :: SourceToken
  , imports :: Array (ImportDecl e)
  }

newtype ModuleBody e = ModuleBody
  { decls :: Array (PSDeclaration e)
  , trailingComments :: Array (Comment LineFeed)
  , end :: SourcePos
  }

derive instance newtypeModuleBody :: Newtype (ModuleBody e) _

data Export e
  = ExportValue (Name Ident)
  | ExportOp (Name Operator)
  | ExportType (Name Proper) (Maybe DataMembers)
  | ExportTypeOp SourceToken (Name Operator)
  | ExportClass SourceToken (Name Proper)
  | ExportKind SourceToken (Name Proper)
  | ExportModule SourceToken (Name ModuleName)
  | ExportError e

data DataMembers
  = DataAll SourceToken
  | DataEnumerated (Delimited (Name Proper))

data PSDeclaration e
  = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
  | DeclType (DataHead e) SourceToken (PSType e)
  | DeclNewtype (DataHead e) SourceToken (Name Proper) (PSType e)
  | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (PSType e)))))
  | DeclInstanceChain (Separated (Instance e))
  | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
  | DeclKindSignature SourceToken (Labeled (Name Proper) (PSType e))
  | DeclSignature (Labeled (Name Ident) (PSType e))
  | DeclValue (ValueBindingFields e)
  | DeclFixity FixityFields
  | DeclForeign SourceToken SourceToken (Foreign e)
  | DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
  | DeclError e

newtype Instance e = Instance
  { head :: InstanceHead e
  , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
  }

derive instance newtypeInstance :: Newtype (Instance e) _

data InstanceBinding e
  = InstanceBindingSignature (Labeled (Name Ident) (PSType e))
  | InstanceBindingName (ValueBindingFields e)

newtype ImportDecl e = ImportDecl
  { keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

data Import e
  = ImportValue (Name Ident)
  | ImportOp (Name Operator)
  | ImportType (Name Proper) (Maybe DataMembers)
  | ImportTypeOp SourceToken (Name Operator)
  | ImportClass SourceToken (Name Proper)
  | ImportKind SourceToken (Name Proper)
  | ImportError e

type DataHead e =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: Array (TypeVarBinding e)
  }

newtype DataCtor e = DataCtor
  { name :: Name Proper
  , fields :: Array (PSType e)
  }

type ClassHead e =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (OneOrDelimited (PSType e)) SourceToken)
  , name :: Name Proper
  , vars :: Array (TypeVarBinding e)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyArray (Name Ident))
  | FundepDetermines (NonEmptyArray (Name Ident)) SourceToken (NonEmptyArray (Name Ident))

type InstanceHead e =
  { keyword :: SourceToken
  , name :: Name Ident
  , separator :: SourceToken
  , constraints :: Maybe (Tuple (OneOrDelimited (PSType e)) SourceToken)
  , className :: QualifiedName Proper
  , types :: Array (PSType e)
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

type ValueBindingFields e =
  { name :: Name Ident
  , binders :: Array (PSBinder e)
  , guarded :: Guarded e
  }

data Guarded e
  = Unconditional SourceToken (Where e)
  | Guarded (NonEmptyArray (GuardedExpr e))

newtype GuardedExpr e = GuardedExpr
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard e)
  , separator :: SourceToken
  , where :: Where e
  }

newtype PatternGuard e = PatternGuard
  { binder :: Maybe (Tuple (PSBinder e) SourceToken)
  , expr :: PSExpr e
  }

data Foreign e
  = ForeignValue (Labeled (Name Ident) (PSType e))
  | ForeignData SourceToken (Labeled (Name Proper) (PSType e))
  | ForeignKind SourceToken (Name Proper)

data Role
  = Nominal
  | Representational
  | Phantom

data PSExpr e
  = ExprHole (Name Ident)
  | ExprSection SourceToken
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName Proper)
  | ExprBoolean SourceToken Boolean
  | ExprChar SourceToken Char
  | ExprString SourceToken String
  | ExprInt SourceToken Int
  | ExprNumber SourceToken Number
  | ExprArray (Delimited (PSExpr e))
  | ExprRecord (Delimited (RecordLabeled (PSExpr e)))
  | ExprParens (Wrapped (PSExpr e))
  | ExprTyped (PSExpr e) SourceToken (PSType e)
  | ExprInfix (PSExpr e) (NonEmptyArray (Tuple (Wrapped (PSExpr e)) (PSExpr e)))
  | ExprOp (PSExpr e) (NonEmptyArray (Tuple (QualifiedName Operator) (PSExpr e)))
  | ExprOpName (QualifiedName Operator)
  | ExprNegate SourceToken (PSExpr e)
  | ExprRecordAccessor (RecordAccessor e)
  | ExprRecordUpdate (PSExpr e) (DelimitedNonEmpty (RecordUpdate e))
  | ExprApp (PSExpr e) (NonEmptyArray (PSExpr e))
  | ExprLambda (Lambda e)
  | ExprIf (IfThenElse e)
  | ExprCase (CaseOf e)
  | ExprLet (LetIn e)
  | ExprDo (DoBlock e)
  | ExprAdo (AdoBlock e)
  | ExprError e

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

data RecordUpdate e
  = RecordUpdateLeaf (Name Label) SourceToken (PSExpr e)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))

type RecordAccessor e =
  { expr :: PSExpr e
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda e =
  { symbol :: SourceToken
  , binders :: NonEmptyArray (PSBinder e)
  , arrow :: SourceToken
  , body :: PSExpr e
  }

type IfThenElse e =
  { keyword :: SourceToken
  , cond :: PSExpr e
  , then :: SourceToken
  , true :: PSExpr e
  , else :: SourceToken
  , false :: PSExpr e
  }

type CaseOf e =
  { keyword :: SourceToken
  , head :: Separated (PSExpr e)
  , of :: SourceToken
  , branches :: NonEmptyArray (Tuple (Separated (PSBinder e)) (Guarded e))
  }

type LetIn e =
  { keyword :: SourceToken
  , bindings :: NonEmptyArray (LetBinding e)
  , in :: SourceToken
  , body :: PSExpr e
  }

newtype Where e = Where
  { expr :: PSExpr e
  , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding e)))
  }

derive instance newtypeWhere :: Newtype (Where e) _

data LetBinding e
  = LetBindingSignature (Labeled (Name Ident) (PSType e))
  | LetBindingName (ValueBindingFields e)
  | LetBindingPattern (PSBinder e) SourceToken (Where e)
  | LetBindingError e

type DoBlock e =
  { keyword :: SourceToken
  , statements :: NonEmptyArray (DoStatement e)
  }

data DoStatement e
  = DoLet SourceToken (NonEmptyArray (LetBinding e))
  | DoDiscard (PSExpr e)
  | DoBind (PSBinder e) SourceToken (PSExpr e)
  | DoError e

type AdoBlock e =
  { keyword :: SourceToken
  , statements :: Array (DoStatement e)
  , in :: SourceToken
  , result :: PSExpr e
  }

data PSBinder e
  = BinderWildcard SourceToken
  | BinderVar (Name Ident)
  | BinderNamed (Name Ident) SourceToken (PSBinder e)
  | BinderConstructor (QualifiedName Proper) (Array (PSBinder e))
  | BinderBoolean SourceToken Boolean
  | BinderChar SourceToken Char
  | BinderString SourceToken String
  | BinderInt (Maybe SourceToken) SourceToken Int
  | BinderNumber (Maybe SourceToken) SourceToken Number
  | BinderArray (Delimited (PSBinder e))
  | BinderRecord (Delimited (RecordLabeled (PSBinder e)))
  | BinderParens (Wrapped (PSBinder e))
  | BinderTyped (PSBinder e) SourceToken (PSType e)
  | BinderOp (PSBinder e) (NonEmptyArray (Tuple (QualifiedName Operator) (PSBinder e)))
  | BinderError e
