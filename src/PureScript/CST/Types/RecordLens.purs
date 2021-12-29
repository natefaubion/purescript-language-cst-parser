module PureScript.CST.Types.RecordLens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_propArg1 :: forall r a. Lens' { arg1 :: a | r } a
_propArg1 = prop (Proxy :: Proxy "arg1")

_propArg2 :: forall r a. Lens' { arg2 :: a | r } a
_propArg2 = prop (Proxy :: Proxy "arg2")

_propArg3 :: forall r a. Lens' { arg3 :: a | r } a
_propArg3 = prop (Proxy :: Proxy "arg3")

_propArg4 :: forall r a. Lens' { arg4 :: a | r } a
_propArg4 = prop (Proxy :: Proxy "arg4")

_propArrow :: forall r a. Lens' { arrow :: a | r } a
_propArrow = prop (Proxy :: Proxy "arrow")

_propBar :: forall r a. Lens' { bar :: a | r } a
_propBar = prop (Proxy :: Proxy "bar")

_propBinder :: forall r a. Lens' { binder :: a | r } a
_propBinder = prop (Proxy :: Proxy "binder")

_propBinders :: forall r a. Lens' { binders :: a | r } a
_propBinders = prop (Proxy :: Proxy "binders")

_propBindings :: forall r a. Lens' { bindings :: a | r } a
_propBindings = prop (Proxy :: Proxy "bindings")

_propBody :: forall r a. Lens' { body :: a | r } a
_propBody = prop (Proxy :: Proxy "body")

_propBranches :: forall r a. Lens' { branches :: a | r } a
_propBranches = prop (Proxy :: Proxy "branches")

_propClassName :: forall r a. Lens' { className :: a | r } a
_propClassName = prop (Proxy :: Proxy "className")

_propClose :: forall r a. Lens' { close :: a | r } a
_propClose = prop (Proxy :: Proxy "close")

_propColumn :: forall r a. Lens' { column :: a | r } a
_propColumn = prop (Proxy :: Proxy "column")

_propCond :: forall r a. Lens' { cond :: a | r } a
_propCond = prop (Proxy :: Proxy "cond")

_propConstraints :: forall r a. Lens' { constraints :: a | r } a
_propConstraints = prop (Proxy :: Proxy "constraints")

_propDecls :: forall r a. Lens' { decls :: a | r } a
_propDecls = prop (Proxy :: Proxy "decls")

_propDot :: forall r a. Lens' { dot :: a | r } a
_propDot = prop (Proxy :: Proxy "dot")

_propElse :: forall r a. Lens' { else :: a | r } a
_propElse = prop (Proxy :: Proxy "else")

_propEnd :: forall r a. Lens' { end :: a | r } a
_propEnd = prop (Proxy :: Proxy "end")

_propExports :: forall r a. Lens' { exports :: a | r } a
_propExports = prop (Proxy :: Proxy "exports")

_propExpr :: forall r a. Lens' { expr :: a | r } a
_propExpr = prop (Proxy :: Proxy "expr")

_propFalse :: forall r a. Lens' { false :: a | r } a
_propFalse = prop (Proxy :: Proxy "false")

_propFields :: forall r a. Lens' { fields :: a | r } a
_propFields = prop (Proxy :: Proxy "fields")

_propFundeps :: forall r a. Lens' { fundeps :: a | r } a
_propFundeps = prop (Proxy :: Proxy "fundeps")

_propGuarded :: forall r a. Lens' { guarded :: a | r } a
_propGuarded = prop (Proxy :: Proxy "guarded")

_propHead :: forall r a. Lens' { head :: a | r } a
_propHead = prop (Proxy :: Proxy "head")

_propHeader :: forall r a. Lens' { header :: a | r } a
_propHeader = prop (Proxy :: Proxy "header")

_propImports :: forall r a. Lens' { imports :: a | r } a
_propImports = prop (Proxy :: Proxy "imports")

_propIn :: forall r a. Lens' { in :: a | r } a
_propIn = prop (Proxy :: Proxy "in")

_propKeyword :: forall r a. Lens' { keyword :: a | r } a
_propKeyword = prop (Proxy :: Proxy "keyword")

_propLabel :: forall r a. Lens' { label :: a | r } a
_propLabel = prop (Proxy :: Proxy "label")

_propLabels :: forall r a. Lens' { labels :: a | r } a
_propLabels = prop (Proxy :: Proxy "labels")

_propLeadingComments :: forall r a. Lens' { leadingComments :: a | r } a
_propLeadingComments = prop (Proxy :: Proxy "leadingComments")

_propLine :: forall r a. Lens' { line :: a | r } a
_propLine = prop (Proxy :: Proxy "line")

_propModule :: forall r a. Lens' { module :: a | r } a
_propModule = prop (Proxy :: Proxy "module")

_propName :: forall r a. Lens' { name :: a | r } a
_propName = prop (Proxy :: Proxy "name")

_propNames :: forall r a. Lens' { names :: a | r } a
_propNames = prop (Proxy :: Proxy "names")

_propOf :: forall r a. Lens' { of :: a | r } a
_propOf = prop (Proxy :: Proxy "of")

_propOpen :: forall r a. Lens' { open :: a | r } a
_propOpen = prop (Proxy :: Proxy "open")

_propOperator :: forall r a. Lens' { operator :: a | r } a
_propOperator = prop (Proxy :: Proxy "operator")

_propPath :: forall r a. Lens' { path :: a | r } a
_propPath = prop (Proxy :: Proxy "path")

_propPatterns :: forall r a. Lens' { patterns :: a | r } a
_propPatterns = prop (Proxy :: Proxy "patterns")

_propPrec :: forall r a. Lens' { prec :: a | r } a
_propPrec = prop (Proxy :: Proxy "prec")

_propQualified :: forall r a. Lens' { qualified :: a | r } a
_propQualified = prop (Proxy :: Proxy "qualified")

_propRange :: forall r a. Lens' { range :: a | r } a
_propRange = prop (Proxy :: Proxy "range")

_propResult :: forall r a. Lens' { result :: a | r } a
_propResult = prop (Proxy :: Proxy "result")

_propSeparator :: forall r a. Lens' { separator :: a | r } a
_propSeparator = prop (Proxy :: Proxy "separator")

_propStart :: forall r a. Lens' { start :: a | r } a
_propStart = prop (Proxy :: Proxy "start")

_propStatements :: forall r a. Lens' { statements :: a | r } a
_propStatements = prop (Proxy :: Proxy "statements")

_propSuper :: forall r a. Lens' { super :: a | r } a
_propSuper = prop (Proxy :: Proxy "super")

_propSymbol :: forall r a. Lens' { symbol :: a | r } a
_propSymbol = prop (Proxy :: Proxy "symbol")

_propTail :: forall r a. Lens' { tail :: a | r } a
_propTail = prop (Proxy :: Proxy "tail")

_propThen :: forall r a. Lens' { then :: a | r } a
_propThen = prop (Proxy :: Proxy "then")

_propToken :: forall r a. Lens' { token :: a | r } a
_propToken = prop (Proxy :: Proxy "token")

_propTrailingComments :: forall r a. Lens' { trailingComments :: a | r } a
_propTrailingComments = prop (Proxy :: Proxy "trailingComments")

_propTrue :: forall r a. Lens' { true :: a | r } a
_propTrue = prop (Proxy :: Proxy "true")

_propTypes :: forall r a. Lens' { types :: a | r } a
_propTypes = prop (Proxy :: Proxy "types")

_propValue :: forall r a. Lens' { value :: a | r } a
_propValue = prop (Proxy :: Proxy "value")

_propVars :: forall r a. Lens' { vars :: a | r } a
_propVars = prop (Proxy :: Proxy "vars")

_propWhere :: forall r a. Lens' { where :: a | r } a
_propWhere = prop (Proxy :: Proxy "where")
