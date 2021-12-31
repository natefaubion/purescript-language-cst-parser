module PureScript.CST.Types.Lens where

import Prelude

import Prim hiding (Row, Type)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (AdoBlock, CaseOf, ClassHead, DataCtor, DataHead, Delimited, DelimitedNonEmpty, DoBlock, FixityFields, GuardedExpr, Ident, IfThenElse, ImportDecl, Instance, InstanceHead, Label, Labeled, Lambda, LetIn, Module, ModuleBody, ModuleHeader, ModuleName, Name, Operator, PatternGuard, Proper, QualifiedName, RecordAccessor, Row, Separated, SourcePos, SourceRange, SourceToken, ValueBindingFields, Where, Wrapped, Binder(..), ClassFundep(..), Comment(..), DataMembers(..), Declaration(..), DoStatement(..), Export(..), Expr(..), Fixity(..), FixityOp(..), Foreign(..), Guarded(..), Import(..), InstanceBinding(..), IntValue(..), LetBinding(..), LineFeed(..), OneOrDelimited(..), RecordLabeled(..), RecordUpdate(..), Role(..), SourceStyle(..), Token(..), Type(..), TypeVarBinding(..))

_ModuleName :: Lens' ModuleName String
_ModuleName = _Newtype

_SourcePos :: Lens' SourcePos
  { line :: Int
  , column :: Int
  }
_SourcePos = identity

_SourceRange :: Lens' SourceRange
  { start :: SourcePos
  , end :: SourcePos
  }
_SourceRange = identity

_Comment :: forall l. Prism' (Comment l) String
_Comment = prism Comment case _ of
  Comment a -> Right a
  other -> Left other

_Space :: forall l. Prism' (Comment l) Int
_Space = prism Space case _ of
  Space a -> Right a
  other -> Left other

_Line :: forall l. Prism' (Comment l) (Tuple l Int)
_Line = prism (\(Tuple a b) -> Line a b) case _ of
  Line a b -> Right (Tuple a b)
  other -> Left other

_LF :: Prism' LineFeed Unit
_LF = prism (const LF) case _ of
  LF -> Right unit
  other -> Left other

_CRLF :: Prism' LineFeed Unit
_CRLF = prism (const CRLF) case _ of
  CRLF -> Right unit
  other -> Left other

_ASCII :: Prism' SourceStyle Unit
_ASCII = prism (const ASCII) case _ of
  ASCII -> Right unit
  other -> Left other

_Unicode :: Prism' SourceStyle Unit
_Unicode = prism (const Unicode) case _ of
  Unicode -> Right unit
  other -> Left other

_SmallInt :: Prism' IntValue Int
_SmallInt = prism SmallInt case _ of
  SmallInt a -> Right a
  other -> Left other

_BigInt :: Prism' IntValue String
_BigInt = prism BigInt case _ of
  BigInt a -> Right a
  other -> Left other

_BigHex :: Prism' IntValue String
_BigHex = prism BigHex case _ of
  BigHex a -> Right a
  other -> Left other

_TokLeftParen :: Prism' Token Unit
_TokLeftParen = prism (const TokLeftParen) case _ of
  TokLeftParen -> Right unit
  other -> Left other

_TokRightParen :: Prism' Token Unit
_TokRightParen = prism (const TokRightParen) case _ of
  TokRightParen -> Right unit
  other -> Left other

_TokLeftBrace :: Prism' Token Unit
_TokLeftBrace = prism (const TokLeftBrace) case _ of
  TokLeftBrace -> Right unit
  other -> Left other

_TokRightBrace :: Prism' Token Unit
_TokRightBrace = prism (const TokRightBrace) case _ of
  TokRightBrace -> Right unit
  other -> Left other

_TokLeftSquare :: Prism' Token Unit
_TokLeftSquare = prism (const TokLeftSquare) case _ of
  TokLeftSquare -> Right unit
  other -> Left other

_TokRightSquare :: Prism' Token Unit
_TokRightSquare = prism (const TokRightSquare) case _ of
  TokRightSquare -> Right unit
  other -> Left other

_TokLeftArrow :: Prism' Token SourceStyle
_TokLeftArrow = prism TokLeftArrow case _ of
  TokLeftArrow a -> Right a
  other -> Left other

_TokRightArrow :: Prism' Token SourceStyle
_TokRightArrow = prism TokRightArrow case _ of
  TokRightArrow a -> Right a
  other -> Left other

_TokRightFatArrow :: Prism' Token SourceStyle
_TokRightFatArrow = prism TokRightFatArrow case _ of
  TokRightFatArrow a -> Right a
  other -> Left other

_TokDoubleColon :: Prism' Token SourceStyle
_TokDoubleColon = prism TokDoubleColon case _ of
  TokDoubleColon a -> Right a
  other -> Left other

_TokForall :: Prism' Token SourceStyle
_TokForall = prism TokForall case _ of
  TokForall a -> Right a
  other -> Left other

_TokEquals :: Prism' Token Unit
_TokEquals = prism (const TokEquals) case _ of
  TokEquals -> Right unit
  other -> Left other

_TokPipe :: Prism' Token Unit
_TokPipe = prism (const TokPipe) case _ of
  TokPipe -> Right unit
  other -> Left other

_TokTick :: Prism' Token Unit
_TokTick = prism (const TokTick) case _ of
  TokTick -> Right unit
  other -> Left other

_TokDot :: Prism' Token Unit
_TokDot = prism (const TokDot) case _ of
  TokDot -> Right unit
  other -> Left other

_TokComma :: Prism' Token Unit
_TokComma = prism (const TokComma) case _ of
  TokComma -> Right unit
  other -> Left other

_TokUnderscore :: Prism' Token Unit
_TokUnderscore = prism (const TokUnderscore) case _ of
  TokUnderscore -> Right unit
  other -> Left other

_TokBackslash :: Prism' Token Unit
_TokBackslash = prism (const TokBackslash) case _ of
  TokBackslash -> Right unit
  other -> Left other

_TokAt :: Prism' Token Unit
_TokAt = prism (const TokAt) case _ of
  TokAt -> Right unit
  other -> Left other

_TokLowerName :: Prism' Token (Tuple (Maybe ModuleName) String)
_TokLowerName = prism (\(Tuple a b) -> TokLowerName a b) case _ of
  TokLowerName a b -> Right (Tuple a b)
  other -> Left other

_TokUpperName :: Prism' Token (Tuple (Maybe ModuleName) String)
_TokUpperName = prism (\(Tuple a b) -> TokUpperName a b) case _ of
  TokUpperName a b -> Right (Tuple a b)
  other -> Left other

_TokOperator :: Prism' Token (Tuple (Maybe ModuleName) String)
_TokOperator = prism (\(Tuple a b) -> TokOperator a b) case _ of
  TokOperator a b -> Right (Tuple a b)
  other -> Left other

_TokSymbolName :: Prism' Token (Tuple (Maybe ModuleName) String)
_TokSymbolName = prism (\(Tuple a b) -> TokSymbolName a b) case _ of
  TokSymbolName a b -> Right (Tuple a b)
  other -> Left other

_TokSymbolArrow :: Prism' Token SourceStyle
_TokSymbolArrow = prism TokSymbolArrow case _ of
  TokSymbolArrow a -> Right a
  other -> Left other

_TokHole :: Prism' Token String
_TokHole = prism TokHole case _ of
  TokHole a -> Right a
  other -> Left other

_TokChar :: Prism' Token (Tuple String Char)
_TokChar = prism (\(Tuple a b) -> TokChar a b) case _ of
  TokChar a b -> Right (Tuple a b)
  other -> Left other

_TokString :: Prism' Token (Tuple String String)
_TokString = prism (\(Tuple a b) -> TokString a b) case _ of
  TokString a b -> Right (Tuple a b)
  other -> Left other

_TokRawString :: Prism' Token String
_TokRawString = prism TokRawString case _ of
  TokRawString a -> Right a
  other -> Left other

_TokInt :: Prism' Token (Tuple String IntValue)
_TokInt = prism (\(Tuple a b) -> TokInt a b) case _ of
  TokInt a b -> Right (Tuple a b)
  other -> Left other

_TokNumber :: Prism' Token (Tuple String Number)
_TokNumber = prism (\(Tuple a b) -> TokNumber a b) case _ of
  TokNumber a b -> Right (Tuple a b)
  other -> Left other

_TokLayoutStart :: Prism' Token Int
_TokLayoutStart = prism TokLayoutStart case _ of
  TokLayoutStart a -> Right a
  other -> Left other

_TokLayoutSep :: Prism' Token Int
_TokLayoutSep = prism TokLayoutSep case _ of
  TokLayoutSep a -> Right a
  other -> Left other

_TokLayoutEnd :: Prism' Token Int
_TokLayoutEnd = prism TokLayoutEnd case _ of
  TokLayoutEnd a -> Right a
  other -> Left other

_SourceToken :: Lens' SourceToken
  { range :: SourceRange
  , leadingComments :: Array (Comment LineFeed)
  , trailingComments :: Array (Comment Void)
  , value :: Token
  }
_SourceToken = identity

_Ident :: Lens' Ident String
_Ident = _Newtype

_Proper :: Lens' Proper String
_Proper = _Newtype

_Label :: Lens' Label String
_Label = _Newtype

_Operator :: Lens' Operator String
_Operator = _Newtype

_Name
  :: forall a
   . Lens' (Name a)
       { token :: SourceToken
       , name :: a
       }
_Name = _Newtype

_QualifiedName
  :: forall a
   . Lens' (QualifiedName a)
       { token :: SourceToken
       , module :: Maybe ModuleName
       , name :: a
       }
_QualifiedName = _Newtype

_Wrapped
  :: forall a
   . Lens' (Wrapped a)
       { open :: SourceToken
       , value :: a
       , close :: SourceToken
       }
_Wrapped = _Newtype

_Separated
  :: forall a
   . Lens' (Separated a)
       { head :: a
       , tail :: Array (Tuple SourceToken a)
       }
_Separated = _Newtype

_Labeled
  :: forall a b
   . Lens' (Labeled a b)
       { label :: a
       , separator :: SourceToken
       , value :: b
       }
_Labeled = _Newtype

_Delimited :: forall a. Lens' (Delimited a) (Wrapped (Maybe (Separated a)))
_Delimited = identity

_DelimitedNonEmpty :: forall a. Lens' (DelimitedNonEmpty a) (Wrapped (Separated a))
_DelimitedNonEmpty = identity

_One :: forall a. Prism' (OneOrDelimited a) a
_One = prism One case _ of
  One a -> Right a
  other -> Left other

_Many :: forall a. Prism' (OneOrDelimited a) (DelimitedNonEmpty a)
_Many = prism Many case _ of
  Many a -> Right a
  other -> Left other

_TypeVar :: forall e. Prism' (Type e) (Name Ident)
_TypeVar = prism TypeVar case _ of
  TypeVar a -> Right a
  other -> Left other

_TypeConstructor :: forall e. Prism' (Type e) (QualifiedName Proper)
_TypeConstructor = prism TypeConstructor case _ of
  TypeConstructor a -> Right a
  other -> Left other

_TypeWildcard :: forall e. Prism' (Type e) SourceToken
_TypeWildcard = prism TypeWildcard case _ of
  TypeWildcard a -> Right a
  other -> Left other

_TypeHole :: forall e. Prism' (Type e) (Name Ident)
_TypeHole = prism TypeHole case _ of
  TypeHole a -> Right a
  other -> Left other

_TypeString :: forall e. Prism' (Type e) (Tuple SourceToken String)
_TypeString = prism (\(Tuple a b) -> TypeString a b) case _ of
  TypeString a b -> Right (Tuple a b)
  other -> Left other

_TypeRow :: forall e. Prism' (Type e) (Wrapped (Row e))
_TypeRow = prism TypeRow case _ of
  TypeRow a -> Right a
  other -> Left other

_TypeRecord :: forall e. Prism' (Type e) (Wrapped (Row e))
_TypeRecord = prism TypeRecord case _ of
  TypeRecord a -> Right a
  other -> Left other

_TypeForall
  :: forall e
   . Prism' (Type e)
       { arg1 :: SourceToken
       , arg2 :: (NonEmptyArray (TypeVarBinding e))
       , arg3 :: SourceToken
       , arg4 :: (Type e)
       }
_TypeForall = prism (\{ arg1, arg2, arg3, arg4 } -> TypeForall arg1 arg2 arg3 arg4) case _ of
  TypeForall arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_TypeKinded
  :: forall e. Prism' (Type e) { arg1 :: (Type e), arg2 :: SourceToken, arg3 :: (Type e) }
_TypeKinded = prism (\{ arg1, arg2, arg3 } -> TypeKinded arg1 arg2 arg3) case _ of
  TypeKinded arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_TypeApp :: forall e. Prism' (Type e) (Tuple (Type e) (NonEmptyArray (Type e)))
_TypeApp = prism (\(Tuple a b) -> TypeApp a b) case _ of
  TypeApp a b -> Right (Tuple a b)
  other -> Left other

_TypeOp
  :: forall e
   . Prism' (Type e) (Tuple (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e))))
_TypeOp = prism (\(Tuple a b) -> TypeOp a b) case _ of
  TypeOp a b -> Right (Tuple a b)
  other -> Left other

_TypeOpName :: forall e. Prism' (Type e) (QualifiedName Operator)
_TypeOpName = prism TypeOpName case _ of
  TypeOpName a -> Right a
  other -> Left other

_TypeArrow :: forall e. Prism' (Type e) { arg1 :: (Type e), arg2 :: SourceToken, arg3 :: (Type e) }
_TypeArrow = prism (\{ arg1, arg2, arg3 } -> TypeArrow arg1 arg2 arg3) case _ of
  TypeArrow arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_TypeArrowName :: forall e. Prism' (Type e) SourceToken
_TypeArrowName = prism TypeArrowName case _ of
  TypeArrowName a -> Right a
  other -> Left other

_TypeConstrained
  :: forall e. Prism' (Type e) { arg1 :: (Type e), arg2 :: SourceToken, arg3 :: (Type e) }
_TypeConstrained = prism (\{ arg1, arg2, arg3 } -> TypeConstrained arg1 arg2 arg3) case _ of
  TypeConstrained arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_TypeParens :: forall e. Prism' (Type e) (Wrapped (Type e))
_TypeParens = prism TypeParens case _ of
  TypeParens a -> Right a
  other -> Left other

_TypeUnaryRow :: forall e. Prism' (Type e) (Tuple SourceToken (Type e))
_TypeUnaryRow = prism (\(Tuple a b) -> TypeUnaryRow a b) case _ of
  TypeUnaryRow a b -> Right (Tuple a b)
  other -> Left other

_TypeError :: forall e. Prism' (Type e) e
_TypeError = prism TypeError case _ of
  TypeError a -> Right a
  other -> Left other

_TypeVarKinded :: forall e. Prism' (TypeVarBinding e) (Wrapped (Labeled (Name Ident) (Type e)))
_TypeVarKinded = prism TypeVarKinded case _ of
  TypeVarKinded a -> Right a
  other -> Left other

_TypeVarName :: forall e. Prism' (TypeVarBinding e) (Name Ident)
_TypeVarName = prism TypeVarName case _ of
  TypeVarName a -> Right a
  other -> Left other

_Row
  :: forall e
   . Lens' (Row e)
       { labels :: Maybe (Separated (Labeled (Name Label) (Type e)))
       , tail :: Maybe (Tuple SourceToken (Type e))
       }
_Row = _Newtype

_Module
  :: forall e
   . Lens' (Module e)
       { header :: ModuleHeader e
       , body :: ModuleBody e
       }
_Module = _Newtype

_ModuleHeader
  :: forall e
   . Lens' (ModuleHeader e)
       { keyword :: SourceToken
       , name :: Name ModuleName
       , exports :: Maybe (DelimitedNonEmpty (Export e))
       , where :: SourceToken
       , imports :: Array (ImportDecl e)
       }
_ModuleHeader = _Newtype

_ModuleBody
  :: forall e
   . Lens' (ModuleBody e)
       { decls :: Array (Declaration e)
       , trailingComments :: Array (Comment LineFeed)
       , end :: SourcePos
       }
_ModuleBody = _Newtype

_ExportValue :: forall e. Prism' (Export e) (Name Ident)
_ExportValue = prism ExportValue case _ of
  ExportValue a -> Right a
  other -> Left other

_ExportOp :: forall e. Prism' (Export e) (Name Operator)
_ExportOp = prism ExportOp case _ of
  ExportOp a -> Right a
  other -> Left other

_ExportType :: forall e. Prism' (Export e) (Tuple (Name Proper) (Maybe DataMembers))
_ExportType = prism (\(Tuple a b) -> ExportType a b) case _ of
  ExportType a b -> Right (Tuple a b)
  other -> Left other

_ExportTypeOp :: forall e. Prism' (Export e) (Tuple SourceToken (Name Operator))
_ExportTypeOp = prism (\(Tuple a b) -> ExportTypeOp a b) case _ of
  ExportTypeOp a b -> Right (Tuple a b)
  other -> Left other

_ExportClass :: forall e. Prism' (Export e) (Tuple SourceToken (Name Proper))
_ExportClass = prism (\(Tuple a b) -> ExportClass a b) case _ of
  ExportClass a b -> Right (Tuple a b)
  other -> Left other

_ExportKind :: forall e. Prism' (Export e) (Tuple SourceToken (Name Proper))
_ExportKind = prism (\(Tuple a b) -> ExportKind a b) case _ of
  ExportKind a b -> Right (Tuple a b)
  other -> Left other

_ExportModule :: forall e. Prism' (Export e) (Tuple SourceToken (Name ModuleName))
_ExportModule = prism (\(Tuple a b) -> ExportModule a b) case _ of
  ExportModule a b -> Right (Tuple a b)
  other -> Left other

_ExportError :: forall e. Prism' (Export e) e
_ExportError = prism ExportError case _ of
  ExportError a -> Right a
  other -> Left other

_DataAll :: Prism' DataMembers SourceToken
_DataAll = prism DataAll case _ of
  DataAll a -> Right a
  other -> Left other

_DataEnumerated :: Prism' DataMembers (Delimited (Name Proper))
_DataEnumerated = prism DataEnumerated case _ of
  DataEnumerated a -> Right a
  other -> Left other

_DeclData
  :: forall e
   . Prism' (Declaration e)
       (Tuple (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e)))))
_DeclData = prism (\(Tuple a b) -> DeclData a b) case _ of
  DeclData a b -> Right (Tuple a b)
  other -> Left other

_DeclType
  :: forall e
   . Prism' (Declaration e) { arg1 :: (DataHead e), arg2 :: SourceToken, arg3 :: (Type e) }
_DeclType = prism (\{ arg1, arg2, arg3 } -> DeclType arg1 arg2 arg3) case _ of
  DeclType arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_DeclNewtype
  :: forall e
   . Prism' (Declaration e)
       { arg1 :: (DataHead e), arg2 :: SourceToken, arg3 :: (Name Proper), arg4 :: (Type e) }
_DeclNewtype = prism (\{ arg1, arg2, arg3, arg4 } -> DeclNewtype arg1 arg2 arg3 arg4) case _ of
  DeclNewtype arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_DeclClass
  :: forall e
   . Prism' (Declaration e)
       ( Tuple (ClassHead e)
           (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e)))))
       )
_DeclClass = prism (\(Tuple a b) -> DeclClass a b) case _ of
  DeclClass a b -> Right (Tuple a b)
  other -> Left other

_DeclInstanceChain :: forall e. Prism' (Declaration e) (Separated (Instance e))
_DeclInstanceChain = prism DeclInstanceChain case _ of
  DeclInstanceChain a -> Right a
  other -> Left other

_DeclDerive
  :: forall e
   . Prism' (Declaration e)
       { arg1 :: SourceToken, arg2 :: (Maybe SourceToken), arg3 :: (InstanceHead e) }
_DeclDerive = prism (\{ arg1, arg2, arg3 } -> DeclDerive arg1 arg2 arg3) case _ of
  DeclDerive arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_DeclKindSignature
  :: forall e. Prism' (Declaration e) (Tuple SourceToken (Labeled (Name Proper) (Type e)))
_DeclKindSignature = prism (\(Tuple a b) -> DeclKindSignature a b) case _ of
  DeclKindSignature a b -> Right (Tuple a b)
  other -> Left other

_DeclSignature :: forall e. Prism' (Declaration e) (Labeled (Name Ident) (Type e))
_DeclSignature = prism DeclSignature case _ of
  DeclSignature a -> Right a
  other -> Left other

_DeclValue :: forall e. Prism' (Declaration e) (ValueBindingFields e)
_DeclValue = prism DeclValue case _ of
  DeclValue a -> Right a
  other -> Left other

_DeclFixity :: forall e. Prism' (Declaration e) FixityFields
_DeclFixity = prism DeclFixity case _ of
  DeclFixity a -> Right a
  other -> Left other

_DeclForeign
  :: forall e
   . Prism' (Declaration e) { arg1 :: SourceToken, arg2 :: SourceToken, arg3 :: (Foreign e) }
_DeclForeign = prism (\{ arg1, arg2, arg3 } -> DeclForeign arg1 arg2 arg3) case _ of
  DeclForeign arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_DeclRole
  :: forall e
   . Prism' (Declaration e)
       { arg1 :: SourceToken
       , arg2 :: SourceToken
       , arg3 :: (Name Proper)
       , arg4 :: (NonEmptyArray (Tuple SourceToken Role))
       }
_DeclRole = prism (\{ arg1, arg2, arg3, arg4 } -> DeclRole arg1 arg2 arg3 arg4) case _ of
  DeclRole arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_DeclError :: forall e. Prism' (Declaration e) e
_DeclError = prism DeclError case _ of
  DeclError a -> Right a
  other -> Left other

_Instance
  :: forall e
   . Lens' (Instance e)
       { head :: InstanceHead e
       , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
       }
_Instance = _Newtype

_InstanceBindingSignature :: forall e. Prism' (InstanceBinding e) (Labeled (Name Ident) (Type e))
_InstanceBindingSignature = prism InstanceBindingSignature case _ of
  InstanceBindingSignature a -> Right a
  other -> Left other

_InstanceBindingName :: forall e. Prism' (InstanceBinding e) (ValueBindingFields e)
_InstanceBindingName = prism InstanceBindingName case _ of
  InstanceBindingName a -> Right a
  other -> Left other

_ImportDecl
  :: forall e
   . Lens' (ImportDecl e)
       { keyword :: SourceToken
       , module :: Name ModuleName
       , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
       , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
       }
_ImportDecl = _Newtype

_ImportValue :: forall e. Prism' (Import e) (Name Ident)
_ImportValue = prism ImportValue case _ of
  ImportValue a -> Right a
  other -> Left other

_ImportOp :: forall e. Prism' (Import e) (Name Operator)
_ImportOp = prism ImportOp case _ of
  ImportOp a -> Right a
  other -> Left other

_ImportType :: forall e. Prism' (Import e) (Tuple (Name Proper) (Maybe DataMembers))
_ImportType = prism (\(Tuple a b) -> ImportType a b) case _ of
  ImportType a b -> Right (Tuple a b)
  other -> Left other

_ImportTypeOp :: forall e. Prism' (Import e) (Tuple SourceToken (Name Operator))
_ImportTypeOp = prism (\(Tuple a b) -> ImportTypeOp a b) case _ of
  ImportTypeOp a b -> Right (Tuple a b)
  other -> Left other

_ImportClass :: forall e. Prism' (Import e) (Tuple SourceToken (Name Proper))
_ImportClass = prism (\(Tuple a b) -> ImportClass a b) case _ of
  ImportClass a b -> Right (Tuple a b)
  other -> Left other

_ImportKind :: forall e. Prism' (Import e) (Tuple SourceToken (Name Proper))
_ImportKind = prism (\(Tuple a b) -> ImportKind a b) case _ of
  ImportKind a b -> Right (Tuple a b)
  other -> Left other

_ImportError :: forall e. Prism' (Import e) e
_ImportError = prism ImportError case _ of
  ImportError a -> Right a
  other -> Left other

_DataHead
  :: forall e
   . Lens' (DataHead e)
       { keyword :: SourceToken
       , name :: Name Proper
       , vars :: Array (TypeVarBinding e)
       }
_DataHead = identity

_DataCtor
  :: forall e
   . Lens' (DataCtor e)
       { name :: Name Proper
       , fields :: Array (Type e)
       }
_DataCtor = _Newtype

_ClassHead
  :: forall e
   . Lens' (ClassHead e)
       { keyword :: SourceToken
       , super :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
       , name :: Name Proper
       , vars :: Array (TypeVarBinding e)
       , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
       }
_ClassHead = identity

_FundepDetermined :: Prism' ClassFundep (Tuple SourceToken (NonEmptyArray (Name Ident)))
_FundepDetermined = prism (\(Tuple a b) -> FundepDetermined a b) case _ of
  FundepDetermined a b -> Right (Tuple a b)
  other -> Left other

_FundepDetermines :: Prism' ClassFundep
  { arg1 :: (NonEmptyArray (Name Ident))
  , arg2 :: SourceToken
  , arg3 :: (NonEmptyArray (Name Ident))
  }
_FundepDetermines = prism (\{ arg1, arg2, arg3 } -> FundepDetermines arg1 arg2 arg3) case _ of
  FundepDetermines arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_InstanceHead
  :: forall e
   . Lens' (InstanceHead e)
       { keyword :: SourceToken
       , name :: Maybe (Tuple (Name Ident) SourceToken)
       , constraints :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
       , className :: QualifiedName Proper
       , types :: Array (Type e)
       }
_InstanceHead = identity

_Infix :: Prism' Fixity Unit
_Infix = prism (const Infix) case _ of
  Infix -> Right unit
  other -> Left other

_Infixl :: Prism' Fixity Unit
_Infixl = prism (const Infixl) case _ of
  Infixl -> Right unit
  other -> Left other

_Infixr :: Prism' Fixity Unit
_Infixr = prism (const Infixr) case _ of
  Infixr -> Right unit
  other -> Left other

_FixityValue :: Prism' FixityOp
  { arg1 :: (QualifiedName (Either Ident Proper)), arg2 :: SourceToken, arg3 :: (Name Operator) }
_FixityValue = prism (\{ arg1, arg2, arg3 } -> FixityValue arg1 arg2 arg3) case _ of
  FixityValue arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_FixityType :: Prism' FixityOp
  { arg1 :: SourceToken
  , arg2 :: (QualifiedName Proper)
  , arg3 :: SourceToken
  , arg4 :: (Name Operator)
  }
_FixityType = prism (\{ arg1, arg2, arg3, arg4 } -> FixityType arg1 arg2 arg3 arg4) case _ of
  FixityType arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_FixityFields :: Lens' FixityFields
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }
_FixityFields = identity

_ValueBindingFields
  :: forall e
   . Lens' (ValueBindingFields e)
       { name :: Name Ident
       , binders :: Array (Binder e)
       , guarded :: Guarded e
       }
_ValueBindingFields = identity

_Unconditional :: forall e. Prism' (Guarded e) (Tuple SourceToken (Where e))
_Unconditional = prism (\(Tuple a b) -> Unconditional a b) case _ of
  Unconditional a b -> Right (Tuple a b)
  other -> Left other

_Guarded :: forall e. Prism' (Guarded e) (NonEmptyArray (GuardedExpr e))
_Guarded = prism Guarded case _ of
  Guarded a -> Right a
  other -> Left other

_GuardedExpr
  :: forall e
   . Lens' (GuardedExpr e)
       { bar :: SourceToken
       , patterns :: Separated (PatternGuard e)
       , separator :: SourceToken
       , where :: Where e
       }
_GuardedExpr = _Newtype

_PatternGuard
  :: forall e
   . Lens' (PatternGuard e)
       { binder :: Maybe (Tuple (Binder e) SourceToken)
       , expr :: Expr e
       }
_PatternGuard = _Newtype

_ForeignValue :: forall e. Prism' (Foreign e) (Labeled (Name Ident) (Type e))
_ForeignValue = prism ForeignValue case _ of
  ForeignValue a -> Right a
  other -> Left other

_ForeignData :: forall e. Prism' (Foreign e) (Tuple SourceToken (Labeled (Name Proper) (Type e)))
_ForeignData = prism (\(Tuple a b) -> ForeignData a b) case _ of
  ForeignData a b -> Right (Tuple a b)
  other -> Left other

_ForeignKind :: forall e. Prism' (Foreign e) (Tuple SourceToken (Name Proper))
_ForeignKind = prism (\(Tuple a b) -> ForeignKind a b) case _ of
  ForeignKind a b -> Right (Tuple a b)
  other -> Left other

_Nominal :: Prism' Role Unit
_Nominal = prism (const Nominal) case _ of
  Nominal -> Right unit
  other -> Left other

_Representational :: Prism' Role Unit
_Representational = prism (const Representational) case _ of
  Representational -> Right unit
  other -> Left other

_Phantom :: Prism' Role Unit
_Phantom = prism (const Phantom) case _ of
  Phantom -> Right unit
  other -> Left other

_ExprHole :: forall e. Prism' (Expr e) (Name Ident)
_ExprHole = prism ExprHole case _ of
  ExprHole a -> Right a
  other -> Left other

_ExprSection :: forall e. Prism' (Expr e) SourceToken
_ExprSection = prism ExprSection case _ of
  ExprSection a -> Right a
  other -> Left other

_ExprIdent :: forall e. Prism' (Expr e) (QualifiedName Ident)
_ExprIdent = prism ExprIdent case _ of
  ExprIdent a -> Right a
  other -> Left other

_ExprConstructor :: forall e. Prism' (Expr e) (QualifiedName Proper)
_ExprConstructor = prism ExprConstructor case _ of
  ExprConstructor a -> Right a
  other -> Left other

_ExprBoolean :: forall e. Prism' (Expr e) (Tuple SourceToken Boolean)
_ExprBoolean = prism (\(Tuple a b) -> ExprBoolean a b) case _ of
  ExprBoolean a b -> Right (Tuple a b)
  other -> Left other

_ExprChar :: forall e. Prism' (Expr e) (Tuple SourceToken Char)
_ExprChar = prism (\(Tuple a b) -> ExprChar a b) case _ of
  ExprChar a b -> Right (Tuple a b)
  other -> Left other

_ExprString :: forall e. Prism' (Expr e) (Tuple SourceToken String)
_ExprString = prism (\(Tuple a b) -> ExprString a b) case _ of
  ExprString a b -> Right (Tuple a b)
  other -> Left other

_ExprInt :: forall e. Prism' (Expr e) (Tuple SourceToken IntValue)
_ExprInt = prism (\(Tuple a b) -> ExprInt a b) case _ of
  ExprInt a b -> Right (Tuple a b)
  other -> Left other

_ExprNumber :: forall e. Prism' (Expr e) (Tuple SourceToken Number)
_ExprNumber = prism (\(Tuple a b) -> ExprNumber a b) case _ of
  ExprNumber a b -> Right (Tuple a b)
  other -> Left other

_ExprArray :: forall e. Prism' (Expr e) (Delimited (Expr e))
_ExprArray = prism ExprArray case _ of
  ExprArray a -> Right a
  other -> Left other

_ExprRecord :: forall e. Prism' (Expr e) (Delimited (RecordLabeled (Expr e)))
_ExprRecord = prism ExprRecord case _ of
  ExprRecord a -> Right a
  other -> Left other

_ExprParens :: forall e. Prism' (Expr e) (Wrapped (Expr e))
_ExprParens = prism ExprParens case _ of
  ExprParens a -> Right a
  other -> Left other

_ExprTyped :: forall e. Prism' (Expr e) { arg1 :: (Expr e), arg2 :: SourceToken, arg3 :: (Type e) }
_ExprTyped = prism (\{ arg1, arg2, arg3 } -> ExprTyped arg1 arg2 arg3) case _ of
  ExprTyped arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_ExprInfix
  :: forall e. Prism' (Expr e) (Tuple (Expr e) (NonEmptyArray (Tuple (Wrapped (Expr e)) (Expr e))))
_ExprInfix = prism (\(Tuple a b) -> ExprInfix a b) case _ of
  ExprInfix a b -> Right (Tuple a b)
  other -> Left other

_ExprOp
  :: forall e
   . Prism' (Expr e) (Tuple (Expr e) (NonEmptyArray (Tuple (QualifiedName Operator) (Expr e))))
_ExprOp = prism (\(Tuple a b) -> ExprOp a b) case _ of
  ExprOp a b -> Right (Tuple a b)
  other -> Left other

_ExprOpName :: forall e. Prism' (Expr e) (QualifiedName Operator)
_ExprOpName = prism ExprOpName case _ of
  ExprOpName a -> Right a
  other -> Left other

_ExprNegate :: forall e. Prism' (Expr e) (Tuple SourceToken (Expr e))
_ExprNegate = prism (\(Tuple a b) -> ExprNegate a b) case _ of
  ExprNegate a b -> Right (Tuple a b)
  other -> Left other

_ExprRecordAccessor :: forall e. Prism' (Expr e) (RecordAccessor e)
_ExprRecordAccessor = prism ExprRecordAccessor case _ of
  ExprRecordAccessor a -> Right a
  other -> Left other

_ExprRecordUpdate
  :: forall e. Prism' (Expr e) (Tuple (Expr e) (DelimitedNonEmpty (RecordUpdate e)))
_ExprRecordUpdate = prism (\(Tuple a b) -> ExprRecordUpdate a b) case _ of
  ExprRecordUpdate a b -> Right (Tuple a b)
  other -> Left other

_ExprApp :: forall e. Prism' (Expr e) (Tuple (Expr e) (NonEmptyArray (Expr e)))
_ExprApp = prism (\(Tuple a b) -> ExprApp a b) case _ of
  ExprApp a b -> Right (Tuple a b)
  other -> Left other

_ExprLambda :: forall e. Prism' (Expr e) (Lambda e)
_ExprLambda = prism ExprLambda case _ of
  ExprLambda a -> Right a
  other -> Left other

_ExprIf :: forall e. Prism' (Expr e) (IfThenElse e)
_ExprIf = prism ExprIf case _ of
  ExprIf a -> Right a
  other -> Left other

_ExprCase :: forall e. Prism' (Expr e) (CaseOf e)
_ExprCase = prism ExprCase case _ of
  ExprCase a -> Right a
  other -> Left other

_ExprLet :: forall e. Prism' (Expr e) (LetIn e)
_ExprLet = prism ExprLet case _ of
  ExprLet a -> Right a
  other -> Left other

_ExprDo :: forall e. Prism' (Expr e) (DoBlock e)
_ExprDo = prism ExprDo case _ of
  ExprDo a -> Right a
  other -> Left other

_ExprAdo :: forall e. Prism' (Expr e) (AdoBlock e)
_ExprAdo = prism ExprAdo case _ of
  ExprAdo a -> Right a
  other -> Left other

_ExprError :: forall e. Prism' (Expr e) e
_ExprError = prism ExprError case _ of
  ExprError a -> Right a
  other -> Left other

_RecordPun :: forall a. Prism' (RecordLabeled a) (Name Ident)
_RecordPun = prism RecordPun case _ of
  RecordPun a -> Right a
  other -> Left other

_RecordField
  :: forall a. Prism' (RecordLabeled a) { arg1 :: (Name Label), arg2 :: SourceToken, arg3 :: a }
_RecordField = prism (\{ arg1, arg2, arg3 } -> RecordField arg1 arg2 arg3) case _ of
  RecordField arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_RecordUpdateLeaf
  :: forall e
   . Prism' (RecordUpdate e) { arg1 :: (Name Label), arg2 :: SourceToken, arg3 :: (Expr e) }
_RecordUpdateLeaf = prism (\{ arg1, arg2, arg3 } -> RecordUpdateLeaf arg1 arg2 arg3) case _ of
  RecordUpdateLeaf arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_RecordUpdateBranch
  :: forall e. Prism' (RecordUpdate e) (Tuple (Name Label) (DelimitedNonEmpty (RecordUpdate e)))
_RecordUpdateBranch = prism (\(Tuple a b) -> RecordUpdateBranch a b) case _ of
  RecordUpdateBranch a b -> Right (Tuple a b)
  other -> Left other

_RecordAccessor
  :: forall e
   . Lens' (RecordAccessor e)
       { expr :: Expr e
       , dot :: SourceToken
       , path :: Separated (Name Label)
       }
_RecordAccessor = identity

_Lambda
  :: forall e
   . Lens' (Lambda e)
       { symbol :: SourceToken
       , binders :: NonEmptyArray (Binder e)
       , arrow :: SourceToken
       , body :: Expr e
       }
_Lambda = identity

_IfThenElse
  :: forall e
   . Lens' (IfThenElse e)
       { keyword :: SourceToken
       , cond :: Expr e
       , then :: SourceToken
       , true :: Expr e
       , else :: SourceToken
       , false :: Expr e
       }
_IfThenElse = identity

_CaseOf
  :: forall e
   . Lens' (CaseOf e)
       { keyword :: SourceToken
       , head :: Separated (Expr e)
       , of :: SourceToken
       , branches :: NonEmptyArray (Tuple (Separated (Binder e)) (Guarded e))
       }
_CaseOf = identity

_LetIn
  :: forall e
   . Lens' (LetIn e)
       { keyword :: SourceToken
       , bindings :: NonEmptyArray (LetBinding e)
       , in :: SourceToken
       , body :: Expr e
       }
_LetIn = identity

_Where
  :: forall e
   . Lens' (Where e)
       { expr :: Expr e
       , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding e)))
       }
_Where = _Newtype

_LetBindingSignature :: forall e. Prism' (LetBinding e) (Labeled (Name Ident) (Type e))
_LetBindingSignature = prism LetBindingSignature case _ of
  LetBindingSignature a -> Right a
  other -> Left other

_LetBindingName :: forall e. Prism' (LetBinding e) (ValueBindingFields e)
_LetBindingName = prism LetBindingName case _ of
  LetBindingName a -> Right a
  other -> Left other

_LetBindingPattern
  :: forall e. Prism' (LetBinding e) { arg1 :: (Binder e), arg2 :: SourceToken, arg3 :: (Where e) }
_LetBindingPattern = prism (\{ arg1, arg2, arg3 } -> LetBindingPattern arg1 arg2 arg3) case _ of
  LetBindingPattern arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_LetBindingError :: forall e. Prism' (LetBinding e) e
_LetBindingError = prism LetBindingError case _ of
  LetBindingError a -> Right a
  other -> Left other

_DoBlock
  :: forall e
   . Lens' (DoBlock e)
       { keyword :: SourceToken
       , statements :: NonEmptyArray (DoStatement e)
       }
_DoBlock = identity

_DoLet :: forall e. Prism' (DoStatement e) (Tuple SourceToken (NonEmptyArray (LetBinding e)))
_DoLet = prism (\(Tuple a b) -> DoLet a b) case _ of
  DoLet a b -> Right (Tuple a b)
  other -> Left other

_DoDiscard :: forall e. Prism' (DoStatement e) (Expr e)
_DoDiscard = prism DoDiscard case _ of
  DoDiscard a -> Right a
  other -> Left other

_DoBind
  :: forall e. Prism' (DoStatement e) { arg1 :: (Binder e), arg2 :: SourceToken, arg3 :: (Expr e) }
_DoBind = prism (\{ arg1, arg2, arg3 } -> DoBind arg1 arg2 arg3) case _ of
  DoBind arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_DoError :: forall e. Prism' (DoStatement e) e
_DoError = prism DoError case _ of
  DoError a -> Right a
  other -> Left other

_AdoBlock
  :: forall e
   . Lens' (AdoBlock e)
       { keyword :: SourceToken
       , statements :: Array (DoStatement e)
       , in :: SourceToken
       , result :: Expr e
       }
_AdoBlock = identity

_BinderWildcard :: forall e. Prism' (Binder e) SourceToken
_BinderWildcard = prism BinderWildcard case _ of
  BinderWildcard a -> Right a
  other -> Left other

_BinderVar :: forall e. Prism' (Binder e) (Name Ident)
_BinderVar = prism BinderVar case _ of
  BinderVar a -> Right a
  other -> Left other

_BinderNamed
  :: forall e. Prism' (Binder e) { arg1 :: (Name Ident), arg2 :: SourceToken, arg3 :: (Binder e) }
_BinderNamed = prism (\{ arg1, arg2, arg3 } -> BinderNamed arg1 arg2 arg3) case _ of
  BinderNamed arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_BinderConstructor :: forall e. Prism' (Binder e) (Tuple (QualifiedName Proper) (Array (Binder e)))
_BinderConstructor = prism (\(Tuple a b) -> BinderConstructor a b) case _ of
  BinderConstructor a b -> Right (Tuple a b)
  other -> Left other

_BinderBoolean :: forall e. Prism' (Binder e) (Tuple SourceToken Boolean)
_BinderBoolean = prism (\(Tuple a b) -> BinderBoolean a b) case _ of
  BinderBoolean a b -> Right (Tuple a b)
  other -> Left other

_BinderChar :: forall e. Prism' (Binder e) (Tuple SourceToken Char)
_BinderChar = prism (\(Tuple a b) -> BinderChar a b) case _ of
  BinderChar a b -> Right (Tuple a b)
  other -> Left other

_BinderString :: forall e. Prism' (Binder e) (Tuple SourceToken String)
_BinderString = prism (\(Tuple a b) -> BinderString a b) case _ of
  BinderString a b -> Right (Tuple a b)
  other -> Left other

_BinderInt
  :: forall e
   . Prism' (Binder e) { arg1 :: (Maybe SourceToken), arg2 :: SourceToken, arg3 :: IntValue }
_BinderInt = prism (\{ arg1, arg2, arg3 } -> BinderInt arg1 arg2 arg3) case _ of
  BinderInt arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_BinderNumber
  :: forall e
   . Prism' (Binder e) { arg1 :: (Maybe SourceToken), arg2 :: SourceToken, arg3 :: Number }
_BinderNumber = prism (\{ arg1, arg2, arg3 } -> BinderNumber arg1 arg2 arg3) case _ of
  BinderNumber arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_BinderArray :: forall e. Prism' (Binder e) (Delimited (Binder e))
_BinderArray = prism BinderArray case _ of
  BinderArray a -> Right a
  other -> Left other

_BinderRecord :: forall e. Prism' (Binder e) (Delimited (RecordLabeled (Binder e)))
_BinderRecord = prism BinderRecord case _ of
  BinderRecord a -> Right a
  other -> Left other

_BinderParens :: forall e. Prism' (Binder e) (Wrapped (Binder e))
_BinderParens = prism BinderParens case _ of
  BinderParens a -> Right a
  other -> Left other

_BinderTyped
  :: forall e. Prism' (Binder e) { arg1 :: (Binder e), arg2 :: SourceToken, arg3 :: (Type e) }
_BinderTyped = prism (\{ arg1, arg2, arg3 } -> BinderTyped arg1 arg2 arg3) case _ of
  BinderTyped arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_BinderOp
  :: forall e
   . Prism' (Binder e)
       (Tuple (Binder e) (NonEmptyArray (Tuple (QualifiedName Operator) (Binder e))))
_BinderOp = prism (\(Tuple a b) -> BinderOp a b) case _ of
  BinderOp a b -> Right (Tuple a b)
  other -> Left other

_BinderError :: forall e. Prism' (Binder e) e
_BinderError = prism BinderError case _ of
  BinderError a -> Right a
  other -> Left other
