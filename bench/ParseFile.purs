module ParseFile where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
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
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  case Array.head args of
    Just fileName -> do
      contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile fileName
      case parseModule contents of
        ParseSucceeded _ -> do
          Console.log "Parse succeeded."
        ParseSucceededWithErrors _ errs ->
          for_ errs $ Console.error <<< printPositionedError
        ParseFailed err ->
          Console.error $ printPositionedError err
    Nothing ->
      Console.log "File path required"

printPositionedError :: PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error
