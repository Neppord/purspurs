module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.TeamCity (teamcityReporter)
import Test.Spec (describe, it)
import Interpreter (interpret_expr)
import Test.Spec.Assertions (shouldEqual)
import Parser (Value(..), parse_expression)

main :: Effect Unit
main = launchAff_ $ runSpec [ teamcityReporter ] do
  let simple_eval expr = interpret_expr {} (parse_expression expr)
  describe "expression interptreter" do
    describe "handle literals" do
      it "handles Boolean" do
        simple_eval "true"
          # shouldEqual (ValueBoolean true)
      it "handles Char" do
        simple_eval "'*'"
          # shouldEqual (ValueChar '*')
      it "handles Number" do
        simple_eval "42.0"
          # shouldEqual (ValueNumber 42.0)
      it "handles Int" do
        simple_eval "1"
          # shouldEqual (ValueInt 1)
      it "handles String" do
        simple_eval "\"Hello World!\""
          # shouldEqual (ValueString "Hello World!")
      it "handles Array" do
        simple_eval "['*', 42, 42.0]"
          # shouldEqual
              ( ValueArray
                  [ ValueChar '*'
                  , ValueInt 42
                  , ValueNumber 42.0
                  ]
              )
