module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.TeamCity (teamcityReporter)
import Test.Spec (describe, it)
import Interpreter (Value(..), Value(ValueArray), interpret_expr)
import Test.Spec.Assertions (shouldEqual)
import PureScript.CST.Types (IntValue(SmallInt))

main :: Effect Unit
main = launchAff_ $ runSpec [ teamcityReporter ] do
  describe "expression interptreter" do
    describe "handle literals" do
      it "handles Boolean" do
        {}
          # interpret_expr "true"
          # shouldEqual (ValueBoolean true)
      it "handles Char" do
        {}
          # interpret_expr "'*'"
          # shouldEqual (ValueChar '*')
      it "handles Number" do
        {}
          # interpret_expr "42.0"
          # shouldEqual (ValueNumber 42.0)
      it "handles Int" do
        {}
          # interpret_expr "1"
          # shouldEqual (ValueInt (SmallInt 1))
      it "handles String" do
        {}
          # interpret_expr "\"Hello World!\""
          # shouldEqual (ValueString "Hello World!")
      it "handles Array" do
        {}
          # interpret_expr "['*', 42, 42.0]"
          # shouldEqual
              ( ValueArray
                  [ ValueChar '*'
                  , ValueInt (SmallInt 42)
                  , ValueNumber 42.0
                  ]
              )
