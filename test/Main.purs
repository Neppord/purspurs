module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe, it)
import Interpreter (evaluate_expr)
import Test.Spec.Assertions (shouldEqual)
import Parser (Expr(..), Value(..), parse_expression)
import Data.Map.Internal (empty, singleton) as Map
import Test.Spec.Reporter.Spec (specReporter)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  let simple_eval expr = evaluate_expr Map.empty (parse_expression expr)
  describe "expression parser" do
    it "parses identifiers" do
        parse_expression "x" # shouldEqual (ExprIdentifier "x")
    it "parses lambda" do
        parse_expression "\\x -> x" # shouldEqual (ExprLambda "x" (ExprIdentifier "x"))
    it "parses app" do
        parse_expression "f x" # shouldEqual (ExprApp (ExprIdentifier "f") (ExprIdentifier "x"))

  describe "expression interptreter" do
    it "handle identifiers" do
        let
            x = ValueInt 42
            ast = parse_expression "x"
            env = Map.singleton "x" x
        evaluate_expr env ast # shouldEqual x
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
