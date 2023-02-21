module ParseFile where

import Prelude

import Data.Array (foldMap)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (ParseError, printParseError)
import PureScript.CST.Lexer (lex)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Print (TokenOption(..), printSourceTokenWithOption)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream, step)
import PureScript.CST.Types (SourceToken)

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 1 <$> liftEffect Process.argv
  let printTokens = (elem "--tokens" || elem "-t") args
  case Array.head args of
    Just fileName -> do
      contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile fileName

      if printTokens then do
        let
          tokens =
            map (foldMap (printSourceTokenWithOption ShowLayout))
              $ tokenStreamToArray
              $ lex contents
        for_ tokens Console.log
      else
        mempty

      case parseModule contents of
        ParseSucceeded _ -> do
          Console.log "Parse succeeded."
        ParseSucceededWithErrors _ errs -> do
          Console.log "Parse succeeded with errors."
          for_ errs $ Console.error <<< printPositionedError
        ParseFailed err -> do
          Console.log "Parse failed."
          Console.error $ printPositionedError err
    Nothing ->
      Console.log "File path required"

printPositionedError :: PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error

tokenStreamToArray :: TokenStream -> Either ParseError (Array SourceToken)
tokenStreamToArray = go []
  where
  go acc = step >>> case _ of
    TokenEOF _ _ ->
      Right acc
    TokenError _ err _ _ ->
      Left err
    TokenCons tok _ next _ ->
      go (Array.snoc acc tok) next
