module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec')
import Test.Interpreter.Expression (spec) as Interpreter.Expression
import Test.Parser.Declaration (spec) as Parser.Declaration
import Test.Parser.Expression (spec) as Parser.Expression
import Test.Interpreter.E2E (spec) as Interpreter.E2E
import Test.Spec.Config (defaultConfig)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(Milliseconds))

main :: Effect Unit
main = launchAff_ $ runSpec' defaultConfig {timeout = Just $ Milliseconds 20000.0} [ specReporter ] do
  Parser.Declaration.spec
  Parser.Expression.spec
  Interpreter.Expression.spec
  Interpreter.E2E.spec


