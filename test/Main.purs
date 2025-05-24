module Test.Main where

import Prelude
import Prim hiding (Type)

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..), parseBinder, parseDecl, parseExpr, parseModule, parseType)
import PureScript.CST.Types (AppSpine(..), Binder, Comment(..), Declaration(..), DoStatement(..), Expr(..), Label(..), Labeled(..), LetBinding(..), LineFeed(..), Module(..), ModuleBody(..), ModuleHeader(..), Name(..), Prefixed(..), RecordLabeled(..), Separated(..), Token(..), Type(..), TypeVarBinding(..), Wrapped(..))

class ParseFor f where
  parseFor :: String -> RecoveredParserResult f

instance ParseFor Module where
  parseFor = parseModule

instance ParseFor Declaration where
  parseFor = parseDecl

instance ParseFor Expr where
  parseFor = parseExpr

instance ParseFor Type where
  parseFor = parseType

instance ParseFor Binder where
  parseFor = parseBinder

assertParse
  :: forall f
   . ParseFor f
  => String
  -> String
  -> (RecoveredParserResult f -> Boolean)
  -> Effect Unit
assertParse name src k = do
  let res = parseFor (trim src)
  unless (k res) do
    Console.error $ "Assertion failed: " <> name
    Process.exit' 1
  where
  trim =
    String.split (Pattern "\n")
      >>> Array.dropWhile String.null
      >>> Array.uncons
      >>> maybe []
        ( \{ head, tail } -> do
            let leadingSpaces = SCU.takeWhile (eq ' ') head
            let trimLine = SCU.drop (SCU.length leadingSpaces)
            Array.cons (trimLine head) (trimLine <$> tail)
        )
      >>> String.joinWith "\n"

main :: Effect Unit
main = do
  assertParse "Recovered do statements"
    """
    do
      foo <- bar
      a b c +
      foo
    """
    case _ of
      ParseSucceededWithErrors (ExprDo { statements }) _
        | [ DoBind _ _ _
          , DoError _
          , DoDiscard _
          ] <- NonEmptyArray.toArray statements ->
            true
      _ ->
        false

  assertParse "Recovered ado statements"
    """
    ado
      foo <- bar
      a b c +
      foo
      in 5
    """
    case _ of
      ParseSucceededWithErrors (ExprAdo { statements }) _
        | [ DoBind _ _ _
          , DoError _
          , DoDiscard _
          ] <- statements ->
            true
      _ ->
        false

  assertParse "Recovered ado last statement"
    """
    ado
      foo <- bar
      a b c +
      in 5
    """
    case _ of
      ParseSucceededWithErrors (ExprAdo { statements }) _
        | [ DoBind _ _ _
          , DoError _
          ] <- statements ->
            true
      _ ->
        false

  assertParse "Recovered ado first statement"
    """
    ado
      a b c +
      foo <- bar
      in 5
    """
    case _ of
      ParseSucceededWithErrors (ExprAdo { statements }) _
        | [ DoError _
          , DoBind _ _ _
          ] <- statements ->
            true
      _ ->
        false

  assertParse "Empty ado in"
    """
    ado in 1
    """
    case _ of
      (ParseSucceeded _ :: RecoveredParserResult Expr) ->
        true
      _ ->
        false

  assertParse "Empty ado \\n in"
    """
    ado
      in 1
    """
    case _ of
      (ParseSucceeded _ :: RecoveredParserResult Expr) ->
        true
      _ ->
        false

  assertParse "Recovered let bindings"
    """
    let
      a = b c +
      b = 42
    in
      a + b
    """
    case _ of
      ParseSucceededWithErrors (ExprLet { bindings }) _
        | [ LetBindingError _
          , LetBindingName _
          ] <- NonEmptyArray.toArray bindings ->
            true
      _ ->
        false

  assertParse "Recovered declarations"
    """
    module Foo where
    a = 42
    {}
    b = 12
    """
    case _ of
      ParseSucceededWithErrors (Module { body: ModuleBody { decls } }) _
        | [ DeclValue _
          , DeclError _
          , DeclValue _
          ] <- decls ->
            true
      _ ->
        false

  assertParse "Failed mismatched parens"
    """
    wat (bad
    """
    case _ of
      (ParseFailed _ :: RecoveredParserResult Expr) ->
        true
      _ ->
        false

  assertParse "Records with raw string labels"
    "{ \"\"\"key\"\"\": val }"
    case _ of
      ParseSucceeded
        ( ExprRecord
            ( Wrapped
                { value: Just
                    ( Separated
                        { head: RecordField
                            ( Name
                                { name: Label "key", token: { value: TokRawString "key" } }
                            )
                            _
                            _
                        }
                    )
                }
            )
        )
      ->
        true
      _ ->
        false

  assertParse "Negative type-level integers"
    """
    cons
      :: forall len len_plus_1 elem
       . Add 1 len len_plus_1
      => Compare len (-1) GT
      => elem
      -> Vect len elem
      -> Vect len_plus_1 elem
    cons elem (Vect arr) = Vect (A.cons elem arr)
    """
    case _ of
      (ParseSucceeded _ :: RecoveredParserResult Declaration) ->
        true
      _ ->
        false

  assertParse "String with Unicode astral code point hex literal"
    """
    "\x10ffff"
    """
    case _ of
      ParseSucceeded (ExprString _ _) ->
        true
      _ ->
        false

  assertParse "Unicode astral code point Char hex literal"
    """
    '\x10ffff'
    """
    case _ of
      (ParseFailed _ :: RecoveredParserResult Expr) ->
        true
      _ ->
        false

  assertParse "Type applications"
    """
    foo @Bar bar @(Baz 42) 42
    """
    case _ of
      (ParseSucceeded (ExprApp _ apps))
        | [ AppType _ _
          , AppTerm _
          , AppType _ _
          , AppTerm _
          ] <- NonEmptyArray.toArray apps ->
            true
      _ ->
        false

  assertParse "Forall visibility"
    """
    forall @a (@b :: Type) c. a -> c
    """
    case _ of
      ParseSucceeded (TypeForall _ binders _ _)
        | [ TypeVarName (Prefixed { prefix: Just _ })
          , TypeVarKinded (Wrapped { value: Labeled { label: Prefixed { prefix: Just _ } } })
          , TypeVarName (Prefixed { prefix: Nothing })
          ] <- NonEmptyArray.toArray binders ->
            true
      _ ->
        false

  assertParse "Kind applications not supported"
    """
    Foo @Bar
    """
    case _ of
      ParseSucceeded (TypeConstructor _) ->
        true
      _ ->
        false

  assertParse "No module shebang"
    """
    -- no shebang
    module Test where
    """
    case _ of
      ParseSucceeded (Module { header: ModuleHeader { keyword } })
        | [ Comment "-- no shebang"
          , Line LF 1
          ] <- keyword.leadingComments ->
            true
      _ ->
        false

  assertParse "Module shebang"
    """
    #! shebang
    module Test where
    """
    case _ of
      ParseSucceeded (Module { header: ModuleHeader { keyword } })
        | [ Comment "#! shebang"
          , Line LF 1
          ] <- keyword.leadingComments ->
            true
      _ ->
        false

  assertParse "Multiple module shebangs"
    """
    #! shebang 1
    #! shebang 2
    #! shebang 3
    -- no shebang
    module Test where
    """
    case _ of
      ParseSucceeded (Module { header: ModuleHeader { keyword } })
        | [ Comment "#! shebang 1"
          , Line LF 1
          , Comment "#! shebang 2"
          , Line LF 1
          , Comment "#! shebang 3"
          , Line LF 1
          , Comment "-- no shebang"
          , Line LF 1
          ] <- keyword.leadingComments ->
            true
      _ ->
        false

  assertParse "Multiple lines between shebangs should fail"
    """
    #! shebang 1

    #! shebang 2
    #! shebang 3
    module Test where
    """
    case _ of
      (ParseFailed _ :: RecoveredParserResult Module) ->
        true
      _ ->
        false

  assertParse "Comments between shebangs should fail"
    """
    #! shebang 1
    -- no shebang
    #! shebang 2
    #! shebang 3
    module Test where
    """
    case _ of
      (ParseFailed _ :: RecoveredParserResult Module) ->
        true
      _ ->
        false
