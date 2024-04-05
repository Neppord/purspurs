module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Interpreter (default_env, evaluate, evaluate_expr, print)
import Parser (Expr(..), Value(..), parse_expression)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import Data.Array (foldl) as Array
import Data.Map.Internal (empty, singleton) as Map
import Test.Parser.Declaration (spec) as Parser.Declaration
import Test.Parser.Expression (spec) as Parser.Expression

run_program :: Array String -> String
run_program program = Array.foldl (\env decl -> evaluate decl env) default_env program
  # print

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  Parser.Declaration.spec
  Parser.Expression.spec

  describe "evaluate program" do
    it "handles literal" do
      run_program [ "1" ] # shouldEqual "1"
    it "handles value declaration" do
      run_program [ "f x = 1" ] # shouldEqual "(\\x -> 1)"
    it "handles let in" do
      run_program [ "f y = y", "let x = 1 in f x" ] # shouldEqual "1"
    it "handles scope" do
      run_program [ "y = 1", "f y = y", "f 2" ] # shouldEqual "2"
      run_program [ "y = 1", "f y x = y", "f 2 3" ] # shouldEqual "2"
    it "handles session with data" do
      run_program [ "data Foo = Bar Int", "Bar 42" ] # shouldEqual "(Bar 42)"

  describe "expression interptreter" do
    let simple_eval expr = evaluate_expr Map.empty (parse_expression expr)
    it "handle identifiers" do
      let
        x = ValueInt 42
        ast = parse_expression "x"
        env = Map.singleton "x" x
      evaluate_expr env ast # shouldEqual x
    it "handle parenthesis" do
      let
        ast = parse_expression "(42)"
        env = Map.empty
      evaluate_expr env ast # shouldEqual (ValueInt 42)
    it "applyes functions" do
      let
        f = ValueLambda "x" Map.empty $ ExprIdentifier "x"
        ast = parse_expression "f 42"
        env = Map.singleton "f" f
      evaluate_expr env ast # shouldEqual (ValueInt 42)
    it "applyes Constructor" do
      let
        ast = parse_expression "Foo 42"
        env = Map.singleton "Foo"
          $ ValueLambda "$0" Map.empty
          $
            ExprConstructor "Foo" [ ExprIdentifier "$0" ]
      evaluate_expr env ast # shouldEqual (ValueConstructor "Foo" [ ValueInt 42 ])

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
