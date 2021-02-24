module BenchFile where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile)
import Node.Process as Process
import Performance.Minibench (benchWith)
import PureScript.CST (parseModule)

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 1 <$> liftEffect Process.argv
  case Array.head args of
    Just fileName -> do
      contents <- liftEffect <<< Buffer.toString UTF8 =<< readFile fileName
      Console.log $ "Benchmarking " <> fileName
      liftEffect $ benchWith 100 \_ -> parseModule contents
    Nothing ->
      Console.log "File path required"
