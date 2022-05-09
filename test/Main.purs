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
import PureScript.CST.Types (Binder, Declaration(..), DoStatement(..), Expr(..), Label(..), LetBinding(..), Module(..), ModuleBody(..), Name(..), RecordLabeled(..), Separated(..), Token(..), Type, Wrapped(..))

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
    Process.exit 1
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
