module PureScript.CST.Parser where

import Prelude

import Control.Alt (alt)
import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Parser.Monad (Parser, eof, fail, lookAhead, many, optional, take, try)
import PureScript.CST.Types (Binder(..), ClassFundep(..), DataCtor, DataMembers(..), Declaration(..), Delimited, DoStatement(..), Export(..), Expr(..), Fixity(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr, Ident(..), Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), Label(..), Labeled(..), LetBinding(..), Module(..), ModuleName(..), Name(..), OneOrDelimited(..), Operator(..), PatternGuard, Proper(..), QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Role(..), Row(..), Separated(..), SourceToken, Token(..), Type(..), TypeVarBinding(..), Where, Wrapped(..))

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

parseModule :: Parser (Module Unit)
parseModule = do
  keyword <- expect (isKeyword "module")
  name <- parseModuleName
  exports <- optional $ parens $ separated (token TokComma) parseExport
  where_ <- expect (isKeyword "where")
  { left: imports, right: decls } <- map separate $ layout $ Left <$> parseImportDecl <|> Right <$> parseDecl
  trailingComments <- eof
  pure $ Module { ann: unit, keyword, name, exports, where: where_, imports, decls, trailingComments }

parseExport :: Parser (Export Unit)
parseExport =
  ExportTypeOp unit <$> expect (isKeyword "type") <*> parseSymbol
    <|> ExportClass unit <$> expect (isKeyword "class") <*> parseProper
    <|> ExportModule unit <$> expect (isKeyword "module") <*> parseModuleName
    <|> try (ExportKind unit <$> expect (isKeyword "kind") <*> parseProper)
    <|> ExportOp unit <$> parseSymbol
    <|> ExportValue unit <$> parseIdent
    <|> ExportType unit <$> parseProper <*> optional parseDataMembers

parseImportDecl :: Parser (ImportDecl Unit)
parseImportDecl = do
  keyword <- expect (isKeyword "import")
  module_ <- parseModuleName
  names <- optional $ Tuple <$> optional (expect (isKeyword "hiding")) <*> parens (separated (token TokComma) parseImport)
  qualified <- optional $ Tuple <$> expect (isKeyword "as") <*> parseModuleName
  pure $ ImportDecl { ann: unit, keyword, "module": module_, names, qualified }

parseImport :: Parser (Import Unit)
parseImport =
  ImportOp unit <$> parseSymbol
    <|> ImportType unit <$> parseProper <*> optional parseDataMembers
    <|> ImportTypeOp unit <$> expect (isKeyword "type") <*> parseSymbol
    <|> ImportClass unit <$> expect (isKeyword "class") <*> parseProper
    <|> ImportKind unit <$> expect (isKeyword "kind") <*> parseProper
    <|> ImportValue unit <$> parseIdent

parseDataMembers :: Parser (DataMembers Unit)
parseDataMembers =
  DataAll unit <$> expect (isKeySymbol "..")
    <|> DataEnumerated unit <$> delimited TokLeftParen TokRightParen TokComma parseProper

parseDecl :: Parser (Declaration Unit)
parseDecl =
  parseDeclData
    <|> parseDeclNewtype
    <|> parseDeclType
    <|> parseDeclClass
    <|> parseDeclInstanceChain
    <|> parseDeclDerive
    <|> parseDeclValue
    <|> parseDeclForeign
    <|> parseDeclFixity

parseDeclKindSignature :: SourceToken -> Name Proper -> Parser (Declaration Unit)
parseDeclKindSignature keyword label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ DeclKindSignature unit keyword $ Labeled { label, separator, value }

parseDeclData :: Parser (Declaration Unit)
parseDeclData = do
  keyword <- expect (isKeyword "data")
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclData1 keyword name

parseDeclData1 :: SourceToken -> Name Proper -> Parser (Declaration Unit)
parseDeclData1 keyword name = do
  vars <- many parseTypeVarBinding
  ctors <- optional (Tuple <$> token TokEquals <*> separated (token TokPipe) parseDataCtor)
  pure $ DeclData unit { keyword, name, vars } ctors

parseDataCtor :: Parser (DataCtor Unit)
parseDataCtor =
  { ann: unit, name: _, fields: _ }
    <$> parseProper
    <*> many parseTypeAtom

parseDeclNewtype :: Parser (Declaration Unit)
parseDeclNewtype = do
  keyword <- expect (isKeyword "newtype")
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclNewtype1 keyword name

parseDeclNewtype1 :: SourceToken -> Name Proper -> Parser (Declaration Unit)
parseDeclNewtype1 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- token TokEquals
  wrapper <- parseProper
  body <- parseTypeAtom
  pure $ DeclNewtype unit { keyword, name, vars } tok wrapper body

parseDeclType :: Parser (Declaration Unit)
parseDeclType = do
  keyword <- expect (isKeyword "type")
  parseDeclRole keyword
    <|> parseDeclType1 keyword

parseDeclType1 :: SourceToken -> Parser (Declaration Unit)
parseDeclType1 keyword = do
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclType2 keyword name

parseDeclType2 :: SourceToken -> Name Proper -> Parser (Declaration Unit)
parseDeclType2 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- token TokEquals
  body <- parseType
  pure $ DeclType unit { keyword, name, vars } tok body

parseDeclRole :: SourceToken -> Parser (Declaration Unit)
parseDeclRole keyword1 = do
  keyword2 <- expect (isKeyword "role")
  name <- parseProper
  roles <- many1 parseRole
  pure $ DeclRole unit keyword1 keyword2 name roles

parseRole :: Parser (Tuple SourceToken Role)
parseRole =
  flip Tuple Representational <$> expect (isKeyword "representational")
    <|> flip Tuple Nominal <$> expect (isKeyword "nominal")
    <|> flip Tuple Phantom <$> expect (isKeyword "phantom")

parseDeclClass :: Parser (Declaration Unit)
parseDeclClass = do
  keyword <- expect (isKeyword "class")
  parseDeclClassSignature keyword
    <|> parseDeclClass1 keyword

parseDeclClassSignature :: SourceToken -> Parser (Declaration Unit)
parseDeclClassSignature keyword = do
  Tuple label separator <- try $ Tuple <$> parseProper <*> expect isDoubleColon
  value <- parseType
  pure $ DeclKindSignature unit keyword $ Labeled { label, separator, value }

parseDeclClass1 :: SourceToken -> Parser (Declaration Unit)
parseDeclClass1 keyword = do
  super <- optional $ try $ Tuple <$> parseClassConstraints parseType5 <*> expect (isKeyOperator "<=" || isKeyOperator "⇐")
  name <- parseProper
  vars <- many parseTypeVarBinding
  fundeps <- optional $ Tuple <$> token TokPipe <*> separated (token TokComma) parseFundep
  members <- optional $ Tuple <$> expect (isKeyword "where") <*> layoutNonEmpty parseClassMember
  pure $ DeclClass unit { keyword, super, name, vars, fundeps } members

parseClassConstraints :: Parser (Type Unit) -> Parser (OneOrDelimited (Type Unit))
parseClassConstraints parseOneConstraint = do
  Many <$> parens (separated (token TokComma) parseType)
    <|> One <$> parseOneConstraint

parseClassMember :: Parser (Labeled (Name Ident) (Type Unit))
parseClassMember = do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseFundep :: Parser ClassFundep
parseFundep =
  FundepDetermined <$> expect isRightArrow <*> many1 parseIdent
    <|> FundepDetermines <$> many1 parseIdent <*> expect isRightArrow <*> many1 parseIdent

parseDeclInstanceChain :: Parser (Declaration Unit)
parseDeclInstanceChain = DeclInstanceChain unit <$> separated parseInstanceChainSeparator parseInstance

parseInstanceChainSeparator :: Parser SourceToken
parseInstanceChainSeparator =
  expect (isKeyword "else")
    <* optional (token TokLayoutSep)

parseInstance :: Parser (Instance Unit)
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

parseInstanceBinding :: Parser (InstanceBinding Unit)
parseInstanceBinding = do
  ident <- parseIdent
  parseInstanceBindingSignature ident
    <|> parseInstanceBindingName ident

parseInstanceBindingSignature :: Name Ident -> Parser (InstanceBinding Unit)
parseInstanceBindingSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ InstanceBindingSignature unit $ Labeled { label, separator, value }

parseInstanceBindingName :: Name Ident -> Parser (InstanceBinding Unit)
parseInstanceBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ InstanceBindingName unit { name, binders, guarded }

parseDeclDerive :: Parser (Declaration Unit)
parseDeclDerive = do
  derive_ <- expect (isKeyword "derive")
  newtype_ <- optional $ expect (isKeyword "newtype")
  keyword <- expect (isKeyword "instance")
  name <- parseIdent
  separator <- expect isDoubleColon
  constraints <- optional $ try $ Tuple <$> parseClassConstraints parseType3 <*> expect isRightFatArrow
  className <- parseQualifiedProper
  types <- many parseTypeAtom
  pure $ DeclDerive unit derive_ newtype_ { keyword, name, separator, constraints, className, types }

parseDeclValue :: Parser (Declaration Unit)
parseDeclValue = do
  ident <- parseIdent
  parseDeclSignature ident
    <|> parseDeclValue1 ident

parseDeclSignature :: Name Ident -> Parser (Declaration Unit)
parseDeclSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ DeclSignature unit $ Labeled { label, separator, value }

parseDeclValue1 :: Name Ident -> Parser (Declaration Unit)
parseDeclValue1 name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ DeclValue unit { name, binders, guarded }

parseDeclForeign :: Parser (Declaration Unit)
parseDeclForeign = do
  keyword1 <- expect (isKeyword "foreign")
  keyword2 <- expect (isKeyword "import")
  foreign_ <- parseForeignData <|> parseForeignKind <|> parseForeignValue
  pure $ DeclForeign unit keyword1 keyword2 foreign_

parseForeignData :: Parser (Foreign Unit)
parseForeignData = do
  keyword <- expect (isKeyword "data")
  label <- parseProper
  separator <- expect isDoubleColon
  value <- parseType
  pure $ ForeignData keyword $ Labeled { label, separator, value }

parseForeignKind :: Parser (Foreign Unit)
parseForeignKind = try $ ForeignKind <$> expect (isKeyword "kind") <*> parseProper

parseForeignValue :: Parser (Foreign Unit)
parseForeignValue = do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ ForeignValue $ Labeled { label, separator, value }

parseDeclFixity :: Parser (Declaration Unit)
parseDeclFixity = do
  keyword <- parseFixityKeyword
  prec <- parseInt
  operator <- parseFixityOp
  pure $ DeclFixity unit { keyword, prec, operator }

parseFixityKeyword :: Parser (Tuple SourceToken Fixity)
parseFixityKeyword =
  flip Tuple Infix <$> expect (isKeyword "infix")
    <|> flip Tuple Infixl <$> expect (isKeyword "infixl")
    <|> flip Tuple Infixr <$> expect (isKeyword "infixr")

parseFixityOp :: Parser FixityOp
parseFixityOp =
  FixityType <$> expect (isKeyword "type") <*> parseQualifiedProper <*> expect (isKeyword "as") <*> parseOperator
    <|> FixityValue <$> parseQualifiedIdentOrProper <*> expect (isKeyword "as") <*> parseOperator

parseType :: Parser (Type Unit)
parseType = defer \_ -> do
  ty <- parseType1
  TypeKinded unit ty <$> expect isDoubleColon <*> parseType
    <|> pure ty

parseType1 :: Parser (Type Unit)
parseType1 = defer \_ -> do
  parseForall
    <|> parseType2

parseType2 :: Parser (Type Unit)
parseType2 = defer \_ -> do
  ty <- parseType3
  TypeArr unit ty <$> expect isRightArrow <*> parseType1
    <|> TypeConstrained unit ty <$> expect isRightFatArrow <*> parseType1
    <|> pure ty

parseType3 :: Parser (Type Unit)
parseType3 = defer \_ -> do
  foldl (\a (Tuple op b) -> TypeOp unit a op b)
    <$> parseType4
    <*> many (Tuple <$> parseQualifiedOperator <*> parseType4)

parseType4 :: Parser (Type Unit)
parseType4 = defer \_ ->
  TypeUnaryRow unit <$> expect (isKeyOperator "#") <*> parseType4
    <|> parseType5

parseType5 :: Parser (Type Unit)
parseType5 = defer \_ ->
  foldl (TypeApp unit)
    <$> parseTypeAtom
    <*> many parseTypeAtom

parseTypeAtom :: Parser (Type Unit)
parseTypeAtom = defer \_ ->
  TypeVar unit <$> parseIdent
    <|> TypeConstructor unit <$> parseQualifiedProper
    <|> uncurry (TypeString unit) <$> parseString
    <|> parseTypeParens
    <|> TypeRecord unit <$> braces parseRow
    <|> TypeOpName unit <$> parseQualifiedSymbol
    <|> TypeHole unit <$> parseHole
    <|> TypeWildcard unit <$> token TokUnderscore
    <|> TypeArrName unit <$> expect isSymbolArrow

parseTypeParens :: Parser (Type Unit)
parseTypeParens = do
  open <- token TokLeftParen
  parseRowParen open
    <|> parseRowTailParen open
    <|> parseKindedVar open
    <|> parseTypeParen open
    <|> parseEmptyRow open

parseRowParen :: SourceToken -> Parser (Type Unit)
parseRowParen open = do
  Tuple label separator <- try $ Tuple <$> parseLabel <*> expect isDoubleColon
  value <- parseType
  rest <- many (Tuple <$> token TokComma <*> parseRowLabel)
  tail <- optional $ Tuple <$> token TokPipe <*> parseType
  close <- token TokRightParen
  pure $ TypeRow unit $ Wrapped
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

parseRowTailParen :: SourceToken -> Parser (Type Unit)
parseRowTailParen open = do
  tail <- Tuple <$> token TokPipe <*> parseType
  close <- token TokRightParen
  pure $ TypeRow unit $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Just tail }
    , close
    }

parseEmptyRow :: SourceToken -> Parser (Type Unit)
parseEmptyRow open = do
  close <- token TokRightParen
  pure $ TypeRow unit $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Nothing }
    , close
    }

parseKindedVar :: SourceToken -> Parser (Type Unit)
parseKindedVar open = do
  Tuple var separator <- try $ Tuple <$> parens (TypeVar unit <$> parseIdent) <*> expect isDoubleColon
  kind <- parseType
  close <- token TokRightParen
  pure $ TypeParens unit $ Wrapped
    { open
    , value: TypeKinded unit (TypeParens unit var) separator kind
    , close
    }

parseTypeParen :: SourceToken -> Parser (Type Unit)
parseTypeParen open = do
  value <- parseType1
  close <- token TokRightParen
  pure $ TypeParens unit $ Wrapped { open, value, close }

parseRow :: Parser (Row Unit)
parseRow = defer \_ -> do
  labels <- optional $ separated (token TokComma) parseRowLabel
  tail <- optional $ Tuple <$> token TokPipe <*> parseType
  pure $ Row { labels, tail }

parseRowLabel :: Parser (Labeled (Name Label) (Type Unit))
parseRowLabel = do
  label <- parseLabel
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseForall :: Parser (Type Unit)
parseForall = defer \_ ->
  TypeForall unit
    <$> expect isForall
    <*> many1 parseTypeVarBinding
    <*> token TokDot
    <*> parseType1

parseTypeVarBinding :: Parser (TypeVarBinding Unit)
parseTypeVarBinding = defer \_ ->
  parseTypeVarKinded
    <|> TypeVarName <$> parseIdent

parseTypeVarKinded :: Parser (TypeVarBinding Unit)
parseTypeVarKinded = TypeVarKinded <$> parens do
  label <- parseIdent
  separator <- expect isDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseExpr :: Parser (Expr Unit)
parseExpr = defer \_ -> do
  expr <- parseExpr1
  ExprTyped unit expr <$> expect isDoubleColon <*> parseType
    <|> pure expr

parseExpr1 :: Parser (Expr Unit)
parseExpr1 = defer \_ ->
  foldl (uncurry <<< ExprOp unit)
    <$> parseExpr2
    <*> many (Tuple <$> parseQualifiedOperator <*> parseExpr2)

parseExpr2 :: Parser (Expr Unit)
parseExpr2 = defer \_ ->
  foldl (uncurry <<< ExprInfix unit)
    <$> parseExpr3
    <*> many (Tuple <$> parseTickExpr <*> parseExpr)

parseTickExpr :: Parser (Wrapped (Expr Unit))
parseTickExpr = do
  open <- token TokTick
  value <- parseTickExpr1
  close <- token TokTick
  pure $ Wrapped { open, value, close }

parseTickExpr1 :: Parser (Expr Unit)
parseTickExpr1 = defer \_ ->
  foldl (uncurry <<< ExprOp unit)
    <$> parseExpr3
    <*> many (Tuple <$> parseQualifiedOperator <*> parseExpr3)

parseExpr3 :: Parser (Expr Unit)
parseExpr3 = defer \_ -> do
  ExprNegate unit <$> expect (isKeyOperator "-") <*> parseExpr3
    <|> parseExpr4

parseExpr4 :: Parser (Expr Unit)
parseExpr4 = defer \_ ->
  foldl (ExprApp unit)
    <$> parseExpr5
    <*> many parseExpr5

parseExpr5 :: Parser (Expr Unit)
parseExpr5 = defer \_ ->
  parseIf
    <|> parseLetIn
    <|> parseLambda
    <|> parseCase
    <|> parseDo
    <|> parseAdo
    <|> parseExpr6

parseIf :: Parser (Expr Unit)
parseIf = do
  keyword <- expect (isKeyword "if")
  cond <- parseExpr
  then_ <- expect (isKeyword "then")
  true_ <- parseExpr
  else_ <- expect (isKeyword "else")
  false_ <- parseExpr
  pure $ ExprIf unit { keyword, cond, then: then_, true: true_, else: else_, false: false_ }

parseLetIn :: Parser (Expr Unit)
parseLetIn = do
  keyword <- expect (isKeyword "let")
  bindings <- layoutNonEmpty parseLetBinding
  in_ <- expect (isKeyword "in")
  body <- parseExpr
  pure $ ExprLet unit { keyword, bindings, in: in_, body }

parseLambda :: Parser (Expr Unit)
parseLambda = do
  symbol <- token TokBackslash
  binders <- many1 parseBinderAtom
  arrow <- expect isRightArrow
  body <- parseExpr
  pure $ ExprLambda unit { symbol, binders, arrow, body }

parseCase :: Parser (Expr Unit)
parseCase = do
  keyword <- expect (isKeyword "case")
  head <- separated (token TokComma) parseExpr
  of_ <- expect (isKeyword "of")
  branches <- try parseBadSingleCaseBranch <|> parseCaseBranches
  pure $ ExprCase unit { keyword, head, of: of_, branches }

parseCaseBranches :: Parser (NonEmptyArray (Tuple (Separated (Binder Unit)) (Guarded Unit)))
parseCaseBranches = defer \_ ->
  layoutNonEmpty $ Tuple <$> separated (token TokComma) parseBinder1 <*> parseGuarded (expect isRightArrow)

parseBadSingleCaseBranch :: Parser (NonEmptyArray (Tuple (Separated (Binder Unit)) (Guarded Unit)))
parseBadSingleCaseBranch = do
  binder <- token TokLayoutStart *> parseBinder1
  parseBadSingleCaseWhere binder
    <|> parseBadSingleCaseGuarded binder

parseBadSingleCaseWhere :: Binder Unit ->Parser (NonEmptyArray (Tuple (Separated (Binder Unit)) (Guarded Unit)))
parseBadSingleCaseWhere binder = do
  arrow <- expect isRightArrow
  body <- token TokLayoutEnd *> parseWhere
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) $ Unconditional arrow body

parseBadSingleCaseGuarded :: Binder Unit ->Parser (NonEmptyArray (Tuple (Separated (Binder Unit)) (Guarded Unit)))
parseBadSingleCaseGuarded binder = do
  body <- token TokLayoutEnd *> parseGuarded (expect isRightArrow)
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) body

parseDo :: Parser (Expr Unit)
parseDo = do
  keyword <- expect (isQualifiedKeyword "do")
  statements <- layoutNonEmpty parseDoStatement
  pure $ ExprDo unit { keyword, statements }

parseAdo :: Parser (Expr Unit)
parseAdo = do
  keyword <- expect (isQualifiedKeyword "ado")
  statements <- layout parseDoStatement
  in_ <- expect (isKeyword "in")
  result <- parseExpr
  pure $ ExprAdo unit { keyword, statements, in: in_, result }

parseExpr6 :: Parser (Expr Unit)
parseExpr6 = defer \_ -> do
  expr <- parseExpr7
  parseRecordUpdates expr
    <|> pure expr

parseRecordUpdates :: Expr Unit -> Parser (Expr Unit)
parseRecordUpdates expr = do
  open <- try $ token TokLeftBrace <* lookAhead (parseLabel *> (token TokEquals <|> token TokLeftBrace))
  value <- separated (token TokComma) parseRecordUpdate
  close <- token TokRightBrace
  pure $ ExprRecordUpdate unit expr $ Wrapped { open, value, close }

parseRecordUpdate :: Parser (RecordUpdate Unit)
parseRecordUpdate = do
  label <- parseLabel
  parseRecordUpdateLeaf label
    <|> parseRecordUpdateBranch label

parseRecordUpdateLeaf :: Name Label -> Parser (RecordUpdate Unit)
parseRecordUpdateLeaf label =
  RecordUpdateLeaf label
    <$> token TokEquals
    <*> parseExpr

parseRecordUpdateBranch :: Name Label -> Parser (RecordUpdate Unit)
parseRecordUpdateBranch label =
  RecordUpdateBranch label
    <$> braces (separated (token TokComma) parseRecordUpdate)

parseExpr7 :: Parser (Expr Unit)
parseExpr7 = defer \_ -> do
  expr <- parseExprAtom
  parseRecordAccessor expr
    <|> pure expr

parseRecordAccessor :: Expr Unit -> Parser (Expr Unit)
parseRecordAccessor expr = do
  dot <- token TokDot
  path <- separated (token TokDot) parseLabel
  pure $ ExprRecordAccessor unit { expr, dot, path }

parseExprAtom :: Parser (Expr Unit)
parseExprAtom = defer \_ ->
  ExprIdent unit <$> parseQualifiedIdent
    <|> ExprConstructor unit <$> parseQualifiedProper
    <|> ExprOpName unit <$> parseQualifiedSymbol
    <|> ExprSection unit <$> token TokUnderscore
    <|> ExprHole unit <$> parseHole
    <|> uncurry (ExprString unit) <$> parseString
    <|> uncurry (ExprChar unit) <$> parseChar
    <|> uncurry (ExprBoolean unit) <$> parseBoolean
    <|> uncurry (ExprInt unit) <$> parseInt
    <|> uncurry (ExprNumber unit) <$> parseNumber
    <|> ExprArray unit <$> delimited TokLeftSquare TokRightSquare TokComma parseExpr
    <|> ExprRecord unit <$> delimited TokLeftBrace TokRightBrace TokComma (parseRecordLabeled parseExpr)
    <|> ExprParens unit <$> parens parseExpr

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

parseDoStatement :: Parser (DoStatement Unit)
parseDoStatement = defer \_ ->
  DoLet <$> expect (isKeyword "let") <*> layoutNonEmpty parseLetBinding
    <|> uncurry DoBind <$> try (Tuple <$> parseBinder <*> expect isLeftArrow) <*> parseExpr
    <|> DoDiscard <$> parseExpr

parseLetBinding :: Parser (LetBinding Unit)
parseLetBinding = defer \_ ->
  try parseIdentBinding
    <|> LetBindingPattern unit <$> parseBinder1 <*> token TokEquals <*> parseWhere

parseIdentBinding :: Parser (LetBinding Unit)
parseIdentBinding = do
  ident <- parseIdent
  parseLetBindingSignature ident
    <|> parseLetBindingName ident

parseLetBindingSignature :: Name Ident -> Parser (LetBinding Unit)
parseLetBindingSignature label = do
  separator <- expect isDoubleColon
  value <- parseType
  pure $ LetBindingSignature unit $ Labeled { label, separator, value }

parseLetBindingName :: Name Ident -> Parser (LetBinding Unit)
parseLetBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (token TokEquals)
  pure $ LetBindingName unit { name, binders, guarded }

parseGuarded :: Parser SourceToken -> Parser (Guarded Unit)
parseGuarded sepParser =
  Unconditional <$> sepParser <*> parseWhere
    <|> Guarded <$> many1 parseGuardedExpr
  where
  parseGuardedExpr :: Parser (GuardedExpr Unit)
  parseGuardedExpr =
    { bar: _, patterns: _, separator: _, where: _ }
      <$> token TokPipe
      <*> separated (token TokComma) parsePatternGuard
      <*> sepParser
      <*> parseWhere

  parsePatternGuard :: Parser (PatternGuard Unit)
  parsePatternGuard =
    { binder: _, expr: _ }
      <$> optional (try (Tuple <$> parseBinder <*> expect isLeftArrow))
      <*> parseExpr

parseWhere :: Parser (Where Unit)
parseWhere = defer \_ ->
  { expr: _, bindings: _ }
    <$> parseExpr
    <*> optional (Tuple <$> expect (isKeyword "where") <*> layoutNonEmpty parseLetBinding)

parseBinder :: Parser (Binder Unit)
parseBinder = defer \_ -> do
  binder <- parseBinder1
  BinderTyped unit binder <$> expect isDoubleColon <*> parseType
    <|> pure binder

parseBinder1 :: Parser (Binder Unit)
parseBinder1 = defer \_ ->
  foldl (\a (Tuple op b) -> BinderOp unit a op b)
    <$> parseBinder2
    <*> many (Tuple <$> parseQualifiedOperator <*> parseBinder2)

parseBinder2 :: Parser (Binder Unit)
parseBinder2 = defer \_ ->
  parseBinderNegative
    <|> parseBinderConstructor

parseBinderNegative :: Parser (Binder Unit)
parseBinderNegative =  do
  negative <- expect (isKeyOperator "-")
  uncurry (BinderInt unit (Just negative)) <$> parseInt
    <|> uncurry (BinderNumber unit (Just negative)) <$> parseNumber

parseBinderConstructor :: Parser (Binder Unit)
parseBinderConstructor = defer \_ -> do
  binder <- parseBinderAtom
  apps <- many parseBinderAtom
  case binder, apps of
    _, [] ->
      pure binder
    BinderConstructor _ name _, _ ->
      pure $ BinderConstructor unit name apps
    _, _ ->
      -- TODO
      fail { line: 0, column: 0 } $ UnexpectedEof

parseBinderAtom :: Parser (Binder Unit)
parseBinderAtom = defer \_ ->
  parseIdentBinder
    <|> flip (BinderConstructor unit) [] <$> parseQualifiedProper
    <|> BinderWildcard unit <$> token TokUnderscore
    <|> uncurry (BinderString unit) <$> parseString
    <|> uncurry (BinderChar unit) <$> parseChar
    <|> uncurry (BinderBoolean unit) <$> parseBoolean
    <|> uncurry (BinderInt unit Nothing) <$> parseInt
    <|> uncurry (BinderNumber unit Nothing) <$>  parseNumber
    <|> BinderArray unit <$> delimited TokLeftSquare TokRightSquare TokComma parseBinder
    <|> BinderRecord unit <$> delimited TokLeftBrace TokRightBrace TokComma (parseRecordLabeled parseBinder)
    <|> BinderParens unit <$> parens parseBinder

parseIdentBinder :: Parser (Binder Unit)
parseIdentBinder = do
  ident <- parseIdent
  BinderNamed unit ident <$> token TokAt <*> parseBinderAtom
    <|> pure (BinderVar unit ident)

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
