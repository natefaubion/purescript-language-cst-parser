module PureScript.CST.Parser
  ( RecoveredError
  , Recovered
  , parseModule
  , parseImportDecl
  , parseType
  , parseExpr
  , parseBinder
  ) where

import Prelude

import Control.Alt (alt)
import Control.Lazy (defer, fix)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Parser.Monad (Parser, PositionedError, Recovery(..), eof, lookAhead, many, optional, recover, take, try)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Binder(..), ClassFundep(..), DataCtor, DataMembers(..), Declaration(..), Delimited, DoStatement(..), Export(..), Expr(..), Fixity(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr, Ident(..), Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), Label(..), Labeled(..), LetBinding(..), Module, ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), OneOrDelimited(..), Operator(..), PatternGuard, Proper(..), QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Role(..), Row(..), Separated(..), SourcePos, SourceToken, Token(..), Type(..), TypeVarBinding(..), Where, Wrapped(..))

type RecoveredError =
  { error :: ParseError
  , position :: SourcePos
  , tokens :: Array SourceToken
  }

type Recovered f = f RecoveredError

type RecoveryStrategy f = Parser (Recovered f) -> Parser (Recovered f)

-- Right associated alts are more efficient for the parser interpreter since
-- it doesn't have to build and walk a stack for each chain greedily, but it
-- can expand them on demand.
infixr 3 alt as <|>

expectMap :: forall a. (SourceToken -> Maybe a) -> Parser a
expectMap k = take \tok ->
  case k tok of
    Just a ->
      Right a
    Nothing ->
      Left $ UnexpectedToken tok.value

expect :: (Token -> Boolean) -> Parser SourceToken
expect pred = expectMap \tok ->
  if pred tok.value then Just tok else Nothing

token :: Token -> Parser SourceToken
token = expect <<< eq

wrapped :: forall a. Token -> Token -> Parser a -> Parser (Wrapped a)
wrapped openTok closeTok valueParser = do
  open <- token openTok
  value <- valueParser
  close <- token closeTok
  pure $ Wrapped { open, value, close }

delimited :: forall a. Token -> Token -> Token-> Parser a -> Parser (Delimited a)
delimited openTok closeTok sepTok valueParser = do
  open <- token openTok
  parseEmpty open
    <|> parseNonEmpty open
  where
  parseEmpty :: SourceToken -> Parser (Delimited a)
  parseEmpty open = ado
    close <- token closeTok
    in Wrapped { open, value: Nothing, close }

  parseNonEmpty :: SourceToken -> Parser (Delimited a)
  parseNonEmpty open = ado
    value <- separated (token sepTok) valueParser
    close <- token closeTok
    in Wrapped { open, value: Just value, close }

separated :: forall a. Parser SourceToken -> Parser a -> Parser (Separated a)
separated sepParser valueParser = ado
  head <- valueParser
  tail <- many (Tuple <$> sepParser <*> valueParser)
  in Separated { head, tail }

parens :: forall a. Parser a -> Parser (Wrapped a)
parens = wrapped TokLeftParen TokRightParen

braces :: forall a. Parser a -> Parser (Wrapped a)
braces = wrapped TokLeftBrace TokRightBrace

squares :: forall a. Parser a -> Parser (Wrapped a)
squares = wrapped TokLeftSquare TokRightSquare

layoutNonEmpty :: forall a. Parser a -> Parser (NonEmptyArray a)
layoutNonEmpty valueParser = ado
  head <- token TokLayoutStart *> valueParser
  tail <- many (token TokLayoutSep *> valueParser) <* token TokLayoutEnd
  in NonEmptyArray.cons' head tail

layout :: forall a. Parser a -> Parser (Array a)
layout valueParser =
  token TokLayoutStart *> values <* token TokLayoutEnd
  where
  values = (go =<< valueParser) <|> pure []
  tail = many (token TokLayoutSep *> valueParser)
  go head = Array.cons head <$> tail

parseModule :: Parser (Recovered Module)
parseModule =
  { header: _, body: _ }
    <$> parseModuleHeader
    <*> parseModuleBody

parseModuleHeader :: Parser (Recovered ModuleHeader)
parseModuleHeader = do
  keyword <- expect (isKeyword "module")
  name <- parseModuleName
  exports <- optional $ parens $ separated (token TokComma) parseExport
  where_ <- expect (isKeyword "where")
  imports <- token TokLayoutStart *> parseModuleImportDecls
  pure $ ModuleHeader { keyword, name, exports, where: where_, imports }

parseModuleBody :: Parser (Recovered ModuleBody)
parseModuleBody = do
  decls <- parseModuleDecls <* token TokLayoutEnd
  trailingComments <- eof
  pure $ ModuleBody { decls, trailingComments }

parseModuleImportDecls :: Parser (Array (Recovered ImportDecl))
parseModuleImportDecls = fix \go ->
  Array.cons
    <$> parseImportDecl
    <*> (token TokLayoutSep *> go <|> lookAhead (token TokLayoutEnd) *> pure [])
    <|> pure []

parseModuleDecls :: Parser (Array (Recovered Declaration))
parseModuleDecls =
  lookAhead (token TokLayoutEnd) *> pure []
    <|> fix \go -> Array.cons <$> recoverDecl parseDecl <*> (token TokLayoutSep *> go <|> pure [])

parseExport :: Parser (Recovered Export)
parseExport =
  ExportTypeOp <$> expect (isKeyword "type") <*> parseSymbol
    <|> ExportClass <$> expect (isKeyword "class") <*> parseProper
    <|> ExportModule <$> expect (isKeyword "module") <*> parseModuleName
    <|> try (ExportKind <$> expect (isKeyword "kind") <*> parseProper)
    <|> ExportOp <$> parseSymbol
    <|> ExportValue <$> parseIdent
    <|> ExportType <$> parseProper <*> optional parseDataMembers

parseImportDecl :: Parser (Recovered ImportDecl)
parseImportDecl = do
  keyword <- expect (isKeyword "import")
  module_ <- parseModuleName
  names <- optional $ Tuple <$> optional (expect (isKeyword "hiding")) <*> parens (separated (token TokComma) parseImport)
  qualified <- optional $ Tuple <$> expect (isKeyword "as") <*> parseModuleName
  pure $ ImportDecl { keyword, "module": module_, names, qualified }

parseImport :: Parser (Recovered Import)
parseImport =
  ImportOp <$> parseSymbol
    <|> ImportType <$> parseProper <*> optional parseDataMembers
    <|> ImportTypeOp <$> expect (isKeyword "type") <*> parseSymbol
    <|> ImportClass <$> expect (isKeyword "class") <*> parseProper
    <|> ImportKind <$> expect (isKeyword "kind") <*> parseProper
    <|> ImportValue <$> parseIdent

parseDataMembers :: Parser DataMembers
parseDataMembers =
  DataAll <$> expect (isKeySymbol "..")
    <|> DataEnumerated <$> delimited TokLeftParen TokRightParen TokComma parseProper

parseDecl :: Parser (Recovered Declaration)
parseDecl = do
  parseDeclData
    <|> parseDeclNewtype
    <|> parseDeclType
    <|> parseDeclClass
    <|> parseDeclInstanceChain
    <|> parseDeclDerive
    <|> parseDeclValue
    <|> parseDeclForeign
    <|> parseDeclFixity

parseDeclKindSignature :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclKindSignature keyword label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ DeclKindSignature keyword $ Labeled { label, separator, value }

parseDeclData :: Parser (Recovered Declaration)
parseDeclData = do
  keyword <- expect (isKeyword "data")
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclData1 keyword name

parseDeclData1 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclData1 keyword name = do
  vars <- many parseTypeVarBinding
  ctors <- optional (Tuple <$> token TokEquals <*> separated (token TokPipe) parseDataCtor)
  pure $ DeclData { keyword, name, vars } ctors

parseDataCtor :: Parser (Recovered DataCtor)
parseDataCtor =
  { name: _, fields: _ }
    <$> parseProper
    <*> many parseTypeAtom

parseDeclNewtype :: Parser (Recovered Declaration)
parseDeclNewtype = do
  keyword <- expect (isKeyword "newtype")
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclNewtype1 keyword name

parseDeclNewtype1 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclNewtype1 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- token TokEquals
  wrapper <- parseProper
  body <- parseTypeAtom
  pure $ DeclNewtype { keyword, name, vars } tok wrapper body

parseDeclType :: Parser (Recovered Declaration)
parseDeclType = do
  keyword <- expect (isKeyword "type")
  parseDeclRole keyword
    <|> parseDeclType1 keyword

parseDeclType1 :: SourceToken -> Parser (Recovered Declaration)
parseDeclType1 keyword = do
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclType2 keyword name

parseDeclType2 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclType2 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- token TokEquals
  body <- parseType
  pure $ DeclType { keyword, name, vars } tok body

parseDeclRole :: SourceToken -> Parser (Recovered Declaration)
parseDeclRole keyword1 = do
  keyword2 <- expect (isKeyword "role")
  name <- parseProper
  roles <- many1 parseRole
  pure $ DeclRole keyword1 keyword2 name roles

parseRole :: Parser (Tuple SourceToken Role)
parseRole =
  flip Tuple Representational <$> expect (isKeyword "representational")
    <|> flip Tuple Nominal <$> expect (isKeyword "nominal")
    <|> flip Tuple Phantom <$> expect (isKeyword "phantom")

parseDeclClass :: Parser (Recovered Declaration)
parseDeclClass = do
  keyword <- expect (isKeyword "class")
  parseDeclClassSignature keyword
    <|> parseDeclClass1 keyword

parseDeclClassSignature :: SourceToken -> Parser (Recovered Declaration)
parseDeclClassSignature keyword = do
  Tuple label separator <- try $ Tuple <$> parseProper <*> expect isDoubleColon
  value <- parseType
  pure $ DeclKindSignature keyword $ Labeled { label, separator, value }

parseDeclClass1 :: SourceToken -> Parser (Recovered Declaration)
parseDeclClass1 keyword = do
  super <- optional $ try $ Tuple <$> parseClassConstraints parseType5 <*> expect (isKeyOperator "<=" || isKeyOperator "⇐")
  name <- parseProper
  vars <- many parseTypeVarBinding
  fundeps <- optional $ Tuple <$> token TokPipe <*> separated (token TokComma) parseFundep
  members <- optional $ Tuple <$> expect (isKeyword "where") <*> layoutNonEmpty parseClassMember
  pure $ DeclClass { keyword, super, name, vars, fundeps } members

parseClassConstraints :: Parser (Recovered Type) -> Parser (OneOrDelimited (Recovered Type))
parseClassConstraints parseOneConstraint = do
  Many <$> parens (separated (token TokComma) parseType)
    <|> One <$> parseOneConstraint

parseClassMember :: Parser (Labeled (Name Ident) (Recovered Type))
parseClassMember = do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseFundep :: Parser ClassFundep
parseFundep =
  FundepDetermined <$> expect isRightArrow <*> many1 parseIdent
    <|> FundepDetermines <$> many1 parseIdent <*> expect isRightArrow <*> many1 parseIdent

parseDeclInstanceChain :: Parser (Recovered Declaration)
parseDeclInstanceChain = DeclInstanceChain <$> separated parseInstanceChainSeparator parseInstance

parseInstanceChainSeparator :: Parser SourceToken
parseInstanceChainSeparator =
  expect (isKeyword "else")
    <* optional (token TokLayoutSep)

parseInstance :: Parser (Recovered Instance)
parseInstance = do
  keyword <- expect (isKeyword "instance")
  name <- parseIdent
  separator <- expect isDoubleColon
  constraints <- optional $ try $ Tuple <$> parseClassConstraints parseType3 <*> expect isRightFatArrow
  className <- parseQualifiedProper
  types <- many parseTypeAtom
  body <- optional $ Tuple <$> expect (isKeyword "where") <*> layoutNonEmpty parseInstanceBinding
  pure $ Instance
    { head: { keyword, name, separator, constraints, className, types }
    , body
    }

parseInstanceBinding :: Parser (Recovered InstanceBinding)
parseInstanceBinding = do
  ident <- parseIdent
  parseInstanceBindingSignature ident
    <|> parseInstanceBindingName ident

parseInstanceBindingSignature :: Name Ident -> Parser (Recovered InstanceBinding)
parseInstanceBindingSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ InstanceBindingSignature $ Labeled { label, separator, value }

parseInstanceBindingName :: Name Ident -> Parser (Recovered InstanceBinding)
parseInstanceBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ InstanceBindingName { name, binders, guarded }

parseDeclDerive :: Parser (Recovered Declaration)
parseDeclDerive = do
  derive_ <- expect (isKeyword "derive")
  newtype_ <- optional $ expect (isKeyword "newtype")
  keyword <- expect (isKeyword "instance")
  name <- parseIdent
  separator <- expect isDoubleColon
  constraints <- optional $ try $ Tuple <$> parseClassConstraints parseType3 <*> expect isRightFatArrow
  className <- parseQualifiedProper
  types <- many parseTypeAtom
  pure $ DeclDerive derive_ newtype_ { keyword, name, separator, constraints, className, types }

parseDeclValue :: Parser (Recovered Declaration)
parseDeclValue = do
  ident <- parseIdent
  parseDeclSignature ident
    <|> parseDeclValue1 ident

parseDeclSignature :: Name Ident -> Parser (Recovered Declaration)
parseDeclSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ DeclSignature $ Labeled { label, separator, value }

parseDeclValue1 :: Name Ident -> Parser (Recovered Declaration)
parseDeclValue1 name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ DeclValue { name, binders, guarded }

parseDeclForeign :: Parser (Recovered Declaration)
parseDeclForeign = do
  keyword1 <- expect (isKeyword "foreign")
  keyword2 <- expect (isKeyword "import")
  foreign_ <- parseForeignData <|> parseForeignKind <|> parseForeignValue
  pure $ DeclForeign keyword1 keyword2 foreign_

parseForeignData :: Parser (Recovered Foreign)
parseForeignData = do
  keyword <- expect (isKeyword "data")
  label <- parseProper
  separator <- expect isDoubleColon
  value <- parseType
  pure $ ForeignData keyword $ Labeled { label, separator, value }

parseForeignKind :: Parser (Recovered Foreign)
parseForeignKind = try $ ForeignKind <$> expect (isKeyword "kind") <*> parseProper

parseForeignValue :: Parser (Recovered Foreign)
parseForeignValue = do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ ForeignValue $ Labeled { label, separator, value }

parseDeclFixity :: Parser (Recovered Declaration)
parseDeclFixity = do
  keyword <- parseFixityKeyword
  prec <- parseInt
  operator <- parseFixityOp
  pure $ DeclFixity { keyword, prec, operator }

parseFixityKeyword :: Parser (Tuple SourceToken Fixity)
parseFixityKeyword =
  flip Tuple Infix <$> expect (isKeyword "infix")
    <|> flip Tuple Infixl <$> expect (isKeyword "infixl")
    <|> flip Tuple Infixr <$> expect (isKeyword "infixr")

parseFixityOp :: Parser FixityOp
parseFixityOp =
  FixityType <$> expect (isKeyword "type") <*> parseQualifiedProper <*> expect (isKeyword "as") <*> parseOperator
    <|> FixityValue <$> parseQualifiedIdentOrProper <*> expect (isKeyword "as") <*> parseOperator

parseType :: Parser (Recovered Type)
parseType = defer \_ -> do
  ty <- parseType1
  TypeKinded ty <$> expect isDoubleColon <*> parseType
    <|> pure ty

parseType1 :: Parser (Recovered Type)
parseType1 = defer \_ -> do
  parseForall
    <|> parseType2

parseType2 :: Parser (Recovered Type)
parseType2 = defer \_ -> do
  ty <- parseType3
  TypeArr ty <$> expect isRightArrow <*> parseType1
    <|> TypeConstrained ty <$> expect isRightFatArrow <*> parseType1
    <|> pure ty

parseType3 :: Parser (Recovered Type)
parseType3 = defer \_ -> do
  foldl (\a (Tuple op b) -> TypeOp a op b)
    <$> parseType4
    <*> many (Tuple <$> parseQualifiedOperator <*> parseType4)

parseType4 :: Parser (Recovered Type)
parseType4 = defer \_ ->
  TypeUnaryRow <$> expect (isKeyOperator "#") <*> parseType4
    <|> parseType5

parseType5 :: Parser (Recovered Type)
parseType5 = defer \_ ->
  foldl TypeApp
    <$> parseTypeAtom
    <*> many parseTypeAtom

parseTypeAtom :: Parser (Recovered Type)
parseTypeAtom = defer \_ ->
  TypeVar <$> parseIdent
    <|> TypeConstructor <$> parseQualifiedProper
    <|> uncurry TypeString <$> parseString
    <|> parseTypeParens
    <|> TypeRecord <$> braces parseRow
    <|> TypeOpName <$> parseQualifiedSymbol
    <|> TypeHole <$> parseHole
    <|> TypeWildcard <$> token TokUnderscore
    <|> TypeArrName <$> expect isSymbolArrow

parseTypeParens :: Parser (Recovered Type)
parseTypeParens = do
  open <- token TokLeftParen
  parseRowParen open
    <|> parseRowTailParen open
    <|> parseKindedVar open
    <|> parseTypeParen open
    <|> parseEmptyRow open

parseRowParen :: SourceToken -> Parser (Recovered Type)
parseRowParen open = do
  Tuple label separator <- try $ Tuple <$> parseLabel <*> expect isDoubleColon
  value <- parseType
  rest <- many (Tuple <$> token TokComma <*> parseRowLabel)
  tail <- optional $ Tuple <$> token TokPipe <*> parseType
  close <- token TokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row
        { labels: Just $ Separated
            { head: Labeled { label, separator, value }
            , tail: rest
            }
        , tail
        }
    , close
    }

parseRowTailParen :: SourceToken -> Parser (Recovered Type)
parseRowTailParen open = do
  tail <- Tuple <$> token TokPipe <*> parseType
  close <- token TokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Just tail }
    , close
    }

parseEmptyRow :: SourceToken -> Parser (Recovered Type)
parseEmptyRow open = do
  close <- token TokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Nothing }
    , close
    }

parseKindedVar :: SourceToken -> Parser (Recovered Type)
parseKindedVar open = do
  Tuple var separator <- try $ Tuple <$> parens (TypeVar <$> parseIdent) <*> expect isDoubleColon
  kind <- parseType
  close <- token TokRightParen
  pure $ TypeParens $ Wrapped
    { open
    , value: TypeKinded (TypeParens var) separator kind
    , close
    }

parseTypeParen :: SourceToken -> Parser (Recovered Type)
parseTypeParen open = do
  value <- parseType1
  close <- token TokRightParen
  pure $ TypeParens $ Wrapped { open, value, close }

parseRow :: Parser (Recovered Row)
parseRow = defer \_ -> do
  labels <- optional $ separated (token TokComma) parseRowLabel
  tail <- optional $ Tuple <$> token TokPipe <*> parseType
  pure $ Row { labels, tail }

parseRowLabel :: Parser (Labeled (Name Label) (Recovered Type))
parseRowLabel = do
  label <- parseLabel
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseForall :: Parser (Recovered Type)
parseForall = defer \_ ->
  TypeForall
    <$> expect isForall
    <*> many1 parseTypeVarBinding
    <*> token TokDot
    <*> parseType1

parseTypeVarBinding :: Parser (Recovered TypeVarBinding)
parseTypeVarBinding = defer \_ ->
  parseTypeVarKinded
    <|> TypeVarName <$> parseIdent

parseTypeVarKinded :: Parser (Recovered TypeVarBinding)
parseTypeVarKinded = TypeVarKinded <$> parens do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseExpr :: Parser (Recovered Expr)
parseExpr = defer \_ -> do
  expr <- parseExpr1
  ExprTyped expr <$> expect isDoubleColon <*> parseType
    <|> pure expr

parseExpr1 :: Parser (Recovered Expr)
parseExpr1 = defer \_ ->
  foldl (uncurry <<< ExprOp)
    <$> parseExpr2
    <*> many (Tuple <$> parseQualifiedOperator <*> parseExpr2)

parseExpr2 :: Parser (Recovered Expr)
parseExpr2 = defer \_ ->
  foldl (uncurry <<< ExprInfix)
    <$> parseExpr3
    <*> many (Tuple <$> parseTickExpr <*> parseExpr)

parseTickExpr :: Parser (Wrapped (Recovered Expr))
parseTickExpr = do
  open <- token TokTick
  value <- parseTickExpr1
  close <- token TokTick
  pure $ Wrapped { open, value, close }

parseTickExpr1 :: Parser (Recovered Expr)
parseTickExpr1 = defer \_ ->
  foldl (uncurry <<< ExprOp)
    <$> parseExpr3
    <*> many (Tuple <$> parseQualifiedOperator <*> parseExpr3)

parseExpr3 :: Parser (Recovered Expr)
parseExpr3 = defer \_ -> do
  ExprNegate <$> expect (isKeyOperator "-") <*> parseExpr3
    <|> parseExpr4

parseExpr4 :: Parser (Recovered Expr)
parseExpr4 = defer \_ ->
  foldl (ExprApp)
    <$> parseExpr5
    <*> many parseExpr5

parseExpr5 :: Parser (Recovered Expr)
parseExpr5 = defer \_ ->
  parseIf
    <|> parseLetIn
    <|> parseLambda
    <|> parseCase
    <|> parseDo
    <|> parseAdo
    <|> parseExpr6

parseIf :: Parser (Recovered Expr)
parseIf = do
  keyword <- expect (isKeyword "if")
  cond <- parseExpr
  then_ <- expect (isKeyword "then")
  true_ <- parseExpr
  else_ <- expect (isKeyword "else")
  false_ <- parseExpr
  pure $ ExprIf { keyword, cond, then: then_, true: true_, else: else_, false: false_ }

parseLetIn :: Parser (Recovered Expr)
parseLetIn = do
  keyword <- expect (isKeyword "let")
  bindings <- layoutNonEmpty parseLetBinding
  in_ <- expect (isKeyword "in")
  body <- parseExpr
  pure $ ExprLet { keyword, bindings, in: in_, body }

parseLambda :: Parser (Recovered Expr)
parseLambda = do
  symbol <- token TokBackslash
  binders <- many1 parseBinderAtom
  arrow <- expect isRightArrow
  body <- parseExpr
  pure $ ExprLambda { symbol, binders, arrow, body }

parseCase :: Parser (Recovered Expr)
parseCase = do
  keyword <- expect (isKeyword "case")
  head <- separated (token TokComma) parseExpr
  of_ <- expect (isKeyword "of")
  branches <- try parseBadSingleCaseBranch <|> parseCaseBranches
  pure $ ExprCase { keyword, head, of: of_, branches }

parseCaseBranches :: Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseCaseBranches = defer \_ ->
  layoutNonEmpty $ Tuple <$> separated (token TokComma) parseBinder1 <*> parseGuarded (expect isRightArrow)

parseBadSingleCaseBranch :: Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseBranch = do
  binder <- token TokLayoutStart *> parseBinder1
  parseBadSingleCaseWhere binder
    <|> parseBadSingleCaseGuarded binder

parseBadSingleCaseWhere :: Recovered Binder -> Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseWhere binder = do
  arrow <- expect isRightArrow
  body <- token TokLayoutEnd *> parseWhere
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) $ Unconditional arrow body

parseBadSingleCaseGuarded :: Recovered Binder -> Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseGuarded binder = do
  body <- token TokLayoutEnd *> parseGuarded (expect isRightArrow)
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) body

parseDo :: Parser (Recovered Expr)
parseDo = do
  keyword <- expect (isQualifiedKeyword "do")
  statements <- layoutNonEmpty parseDoStatement
  pure $ ExprDo { keyword, statements }

parseAdo :: Parser (Recovered Expr)
parseAdo = do
  keyword <- expect (isQualifiedKeyword "ado")
  statements <- layout parseDoStatement
  in_ <- expect (isKeyword "in")
  result <- parseExpr
  pure $ ExprAdo { keyword, statements, in: in_, result }

parseExpr6 :: Parser (Recovered Expr)
parseExpr6 = defer \_ -> do
  expr <- parseExpr7
  parseRecordUpdates expr
    <|> pure expr

parseRecordUpdates :: Recovered Expr -> Parser (Recovered Expr)
parseRecordUpdates expr = do
  open <- try $ token TokLeftBrace <* lookAhead (parseLabel *> (token TokEquals <|> token TokLeftBrace))
  value <- separated (token TokComma) parseRecordUpdate
  close <- token TokRightBrace
  pure $ ExprRecordUpdate expr $ Wrapped { open, value, close }

parseRecordUpdate :: Parser (Recovered RecordUpdate)
parseRecordUpdate = do
  label <- parseLabel
  parseRecordUpdateLeaf label
    <|> parseRecordUpdateBranch label

parseRecordUpdateLeaf :: Name Label -> Parser (Recovered RecordUpdate)
parseRecordUpdateLeaf label =
  RecordUpdateLeaf label
    <$> token TokEquals
    <*> parseExpr

parseRecordUpdateBranch :: Name Label -> Parser (Recovered RecordUpdate)
parseRecordUpdateBranch label =
  RecordUpdateBranch label
    <$> braces (separated (token TokComma) parseRecordUpdate)

parseExpr7 :: Parser (Recovered Expr)
parseExpr7 = defer \_ -> do
  expr <- parseExprAtom
  parseRecordAccessor expr
    <|> pure expr

parseRecordAccessor :: Recovered Expr -> Parser (Recovered Expr)
parseRecordAccessor expr = do
  dot <- token TokDot
  path <- separated (token TokDot) parseLabel
  pure $ ExprRecordAccessor { expr, dot, path }

parseExprAtom :: Parser (Recovered Expr)
parseExprAtom = defer \_ ->
  ExprIdent <$> parseQualifiedIdent
    <|> ExprConstructor <$> parseQualifiedProper
    <|> ExprOpName <$> parseQualifiedSymbol
    <|> ExprSection <$> token TokUnderscore
    <|> ExprHole <$> parseHole
    <|> uncurry ExprString <$> parseString
    <|> uncurry ExprChar <$> parseChar
    <|> uncurry ExprBoolean <$> parseBoolean
    <|> uncurry ExprInt <$> parseInt
    <|> uncurry ExprNumber <$> parseNumber
    <|> ExprArray <$> delimited TokLeftSquare TokRightSquare TokComma parseExpr
    <|> ExprRecord <$> delimited TokLeftBrace TokRightBrace TokComma (parseRecordLabeled parseExpr)
    <|> ExprParens <$> parens parseExpr

parseRecordLabeled :: forall a. Parser a -> Parser (RecordLabeled a)
parseRecordLabeled valueParser =
  parseRecordField
    <|> RecordPun <$> parseIdent
  where
  parseRecordField :: Parser (RecordLabeled a)
  parseRecordField =
    uncurry RecordField
      <$> try (Tuple <$> parseLabel <*> expect (isKeyOperator ":"))
      <*> valueParser

parseDoStatement :: Parser (Recovered DoStatement)
parseDoStatement = defer \_ ->
  DoLet <$> expect (isKeyword "let") <*> layoutNonEmpty parseLetBinding
    <|> uncurry DoBind <$> try (Tuple <$> parseBinder <*> expect isLeftArrow) <*> parseExpr
    <|> DoDiscard <$> parseExpr

parseLetBinding :: Parser (Recovered LetBinding)
parseLetBinding = defer \_ ->
  try parseIdentBinding
    <|> LetBindingPattern <$> parseBinder1 <*> token TokEquals <*> parseWhere

parseIdentBinding :: Parser (Recovered LetBinding)
parseIdentBinding = do
  ident <- parseIdent
  parseLetBindingSignature ident
    <|> parseLetBindingName ident

parseLetBindingSignature :: Name Ident -> Parser (Recovered LetBinding)
parseLetBindingSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ LetBindingSignature $ Labeled { label, separator, value }

parseLetBindingName :: Name Ident -> Parser (Recovered LetBinding)
parseLetBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ LetBindingName { name, binders, guarded }

parseGuarded :: Parser SourceToken -> Parser (Recovered Guarded)
parseGuarded sepParser =
  Unconditional <$> sepParser <*> parseWhere
    <|> Guarded <$> many1 parseGuardedExpr
  where
  parseGuardedExpr :: Parser (Recovered GuardedExpr)
  parseGuardedExpr =
    { bar: _, patterns: _, separator: _, where: _ }
      <$> token TokPipe
      <*> separated (token TokComma) parsePatternGuard
      <*> sepParser
      <*> parseWhere

  parsePatternGuard :: Parser (Recovered PatternGuard)
  parsePatternGuard =
    { binder: _, expr: _ }
      <$> optional (try (Tuple <$> parseBinder <*> expect isLeftArrow))
      <*> parseExpr

parseWhere :: Parser (Recovered Where)
parseWhere = defer \_ ->
  { expr: _, bindings: _ }
    <$> parseExpr
    <*> optional (Tuple <$> expect (isKeyword "where") <*> layoutNonEmpty parseLetBinding)

parseBinder :: Parser (Recovered Binder)
parseBinder = defer \_ -> do
  binder <- parseBinder1
  BinderTyped binder <$> expect isDoubleColon <*> parseType
    <|> pure binder

parseBinder1 :: Parser (Recovered Binder)
parseBinder1 = defer \_ ->
  foldl (\a (Tuple op b) -> BinderOp a op b)
    <$> parseBinder2
    <*> many (Tuple <$> parseQualifiedOperator <*> parseBinder2)

parseBinder2 :: Parser (Recovered Binder)
parseBinder2 = defer \_ ->
  parseBinderNegative
    <|> parseBinderConstructor
    <|> parseBinderAtom

parseBinderNegative :: Parser (Recovered Binder)
parseBinderNegative =  do
  negative <- expect (isKeyOperator "-")
  uncurry (BinderInt (Just negative)) <$> parseInt
    <|> uncurry (BinderNumber (Just negative)) <$> parseNumber

parseBinderConstructor :: Parser (Recovered Binder)
parseBinderConstructor = defer \_ -> do
  name <- parseQualifiedProper
  apps <- many parseBinderAtom
  pure $ BinderConstructor name apps

parseBinderAtom :: Parser (Recovered Binder)
parseBinderAtom = defer \_ ->
  parseIdentBinder
    <|> flip BinderConstructor [] <$> parseQualifiedProper
    <|> BinderWildcard <$> token TokUnderscore
    <|> uncurry BinderString <$> parseString
    <|> uncurry BinderChar <$> parseChar
    <|> uncurry BinderBoolean <$> parseBoolean
    <|> uncurry (BinderInt Nothing) <$> parseInt
    <|> uncurry (BinderNumber Nothing) <$>  parseNumber
    <|> BinderArray <$> delimited TokLeftSquare TokRightSquare TokComma parseBinder
    <|> BinderRecord <$> delimited TokLeftBrace TokRightBrace TokComma (parseRecordLabeled parseBinder)
    <|> BinderParens <$> parens parseBinder

parseIdentBinder :: Parser (Recovered Binder)
parseIdentBinder = do
  ident <- parseIdent
  BinderNamed ident <$> token TokAt <*> parseBinderAtom
    <|> pure (BinderVar ident)

parseLabel :: Parser (Name Label)
parseLabel = expectMap case _ of
  tok@{ value: TokString _ label } ->
    Just $ Name { token: tok, name: Label label }
  tok@{ value: TokLowerName Nothing label } ->
    Just $ Name { token: tok, name: Label label }
  _ -> Nothing

parseIdent :: Parser (Name Ident)
parseIdent = expectMap case _ of
  tok@{ value: TokLowerName Nothing ident } | not $ Set.member ident reservedKeywords ->
    Just $ Name { token: tok, name: Ident ident }
  _ -> Nothing

parseQualifiedIdent :: Parser (QualifiedName Ident)
parseQualifiedIdent = expectMap case _ of
  tok@{ value: TokLowerName mn ident } | not $ Set.member ident reservedKeywords ->
    Just $ QualifiedName { token: tok, "module": mn, name: Ident ident }
  _ -> Nothing

parseProper :: Parser (Name Proper)
parseProper = expectMap case _ of
  tok@{ value: TokUpperName Nothing proper } ->
    Just $ Name { token: tok, name: Proper proper }
  _ -> Nothing

parseQualifiedProper :: Parser (QualifiedName Proper)
parseQualifiedProper = expectMap case _ of
  tok@{ value: TokUpperName mn proper } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Proper proper }
  _ -> Nothing

parseQualifiedIdentOrProper :: Parser (QualifiedName (Either Ident Proper))
parseQualifiedIdentOrProper = expectMap case _ of
  tok@{ value: TokLowerName mn ident } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Left $ Ident ident }
  tok@{ value: TokUpperName mn proper } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Right $ Proper proper }
  _ -> Nothing

parseModuleName :: Parser (Name ModuleName)
parseModuleName = expectMap case _ of
  tok@{ value: TokUpperName (Just (ModuleName mn)) proper } ->
    Just $ Name { token: tok, name: ModuleName $ mn <> "." <> proper }
  tok@{ value: TokUpperName Nothing proper } ->
    Just $ Name { token: tok, name: ModuleName proper }
  _ -> Nothing

parseOperator :: Parser (Name Operator)
parseOperator = expectMap case _ of
  tok@{ value: TokOperator Nothing operator } ->
    Just $ Name { token: tok, name: Operator operator }
  _ -> Nothing

parseQualifiedOperator :: Parser (QualifiedName Operator)
parseQualifiedOperator = expectMap case _ of
  tok@{ value: TokOperator mn operator } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Operator operator }
  _ -> Nothing

parseSymbol :: Parser (Name Operator)
parseSymbol = expectMap case _ of
  tok@{ value: TokSymbolName Nothing operator } ->
    Just $ Name { token: tok, name: Operator operator }
  _ -> Nothing

parseQualifiedSymbol :: Parser (QualifiedName Operator)
parseQualifiedSymbol = expectMap case _ of
  tok@{ value: TokSymbolName mn operator } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Operator operator }
  _ -> Nothing

parseHole :: Parser (Name Ident)
parseHole = expectMap case _ of
  tok@{ value: TokHole hole } ->
    Just $ Name { token: tok, name: Ident hole }
  _ -> Nothing

parseString :: Parser (Tuple SourceToken String)
parseString = expectMap case _ of
  tok@{ value: TokString _ str } ->
    Just $ Tuple tok str
  tok@{ value: TokRawString str } ->
    Just $ Tuple tok str
  _ -> Nothing

parseChar :: Parser (Tuple SourceToken Char)
parseChar = expectMap case _ of
  tok@{ value: TokChar _ ch } ->
    Just $ Tuple tok ch
  _ -> Nothing

parseInt :: Parser (Tuple SourceToken Int)
parseInt = expectMap case _ of
  tok@{ value: TokInt _ int } ->
    Just $ Tuple tok int
  _ -> Nothing

parseNumber :: Parser (Tuple SourceToken Number)
parseNumber = expectMap case _ of
  tok@{ value: TokNumber _ number } ->
    Just $ Tuple tok number
  _ -> Nothing

parseBoolean :: Parser (Tuple SourceToken Boolean)
parseBoolean = expectMap case _ of
  tok@{ value: TokLowerName Nothing "true" } ->
    Just $ Tuple tok true
  tok@{ value: TokLowerName Nothing "false" } ->
    Just $ Tuple tok false
  _ -> Nothing

many1 :: forall a. Parser a -> Parser (NonEmptyArray a)
many1 parser =
  NonEmptyArray.cons'
    <$> parser
    <*> many parser

isDoubleColon :: Token -> Boolean
isDoubleColon = case _ of
  TokDoubleColon _ -> true
  _ -> false

isForall :: Token -> Boolean
isForall = case _ of
  TokForall _ -> true
  _ -> false

isRightFatArrow :: Token -> Boolean
isRightFatArrow = case _ of
  TokRightFatArrow _ -> true
  _ -> false

isRightArrow :: Token -> Boolean
isRightArrow = case _ of
  TokRightArrow _ -> true
  _ -> false

isLeftArrow :: Token -> Boolean
isLeftArrow = case _ of
  TokLeftArrow _ -> true
  _ -> false

isSymbolArrow :: Token -> Boolean
isSymbolArrow = case _ of
  TokSymbolArrow _ -> true
  _ -> false

isKeyword :: String -> Token -> Boolean
isKeyword kw = case _ of
  TokLowerName Nothing name -> kw == name
  _ -> false

isQualifiedKeyword :: String -> Token -> Boolean
isQualifiedKeyword kw = case _ of
  TokLowerName _ name -> kw == name
  _ -> false

isKeyOperator :: String -> Token -> Boolean
isKeyOperator sym = case _ of
  TokOperator Nothing name -> sym == name
  _ -> false

isKeySymbol :: String -> Token -> Boolean
isKeySymbol sym = case _ of
  TokSymbolName Nothing name -> sym == name
  _ -> false

reservedKeywords :: Set String
reservedKeywords = Set.fromFoldable
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "true"
  , "type"
  , "where"
  ]

recoverTopLevelLayout :: forall a. (PositionedError -> Array SourceToken -> a) -> Parser a -> Parser a
recoverTopLevelLayout mkNode = recover \err ->
  map (mkNode err) <<< recoverTokensWhile \tok ->
    case tok.value of
      TokLayoutEnd
        | tok.range.start.column == 0 -> false
      TokLayoutSep
        | tok.range.start.column == 0 -> false
      _ -> true

recoverTokensWhile :: (SourceToken -> Boolean) -> TokenStream -> Recovery (Array SourceToken)
recoverTokensWhile p = go []
  where
  go :: Array SourceToken -> TokenStream -> Recovery (Array SourceToken)
  go acc stream = case TokenStream.step stream of
    TokenError errPos err errStream ->
      Recovery acc errPos stream
    TokenEOF eofPos _ ->
      Recovery acc eofPos stream
    TokenCons tok nextPos nextStream
      | p tok ->
          go (Array.snoc acc tok) nextStream
      | otherwise ->
          Recovery acc tok.range.start stream

recoverDecl :: RecoveryStrategy Declaration
recoverDecl = recoverTopLevelLayout \{ error, position } tokens ->
  DeclError { error, position, tokens }
