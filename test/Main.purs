module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Interpreter (default_env, evaluate, evaluate_expr)
import Parser (Declaration(..), Expr(..), Value(..), parse_declaration, parse_expression)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import Data.Map.Internal (empty, fromFoldable, lookup, singleton) as Map
import Data.Array (foldl)
import Data.Array (foldl, foldr) as Array
import Data.Maybe (fromMaybe, maybe)

run_program :: Array String -> String
run_program program = Array.foldl (\ env decl -> evaluate decl env) default_env program
  # (Map.lookup "_")
  # fromMaybe ValueError
  # show

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  describe "evaluate program" do
    it "handles let in" do
      run_program [ "f y = y", "let x = 1 in f x" ] # shouldEqual "1"
    it "handles session with data" do
      default_env
        # evaluate "data Foo = Bar Int"
        # evaluate "Bar 42"
        # shouldEqual
            ( Map.fromFoldable
                [ "Bar" /\ ValueLambda "$0" (ExprConstructor "Bar" [ ExprIdentifier "$0" ])
                , "_" /\ ValueConstructor "Bar" [ ValueInt 42 ]
                ]
            )
  describe "declaration parser" do
    it "parses data declaration with one constructor" do
      parse_declaration "data Command = Noop"
        # shouldEqual
            ( DeclarationData "Command"
                [ "Noop" /\ ValueConstructor "Noop" []
                ]
            )
    it "parses data declaration with one constructor with two parameter" do
      parse_declaration "data Command = Add Int Int"
        # shouldEqual
            ( DeclarationData "Command"
                [ "Add" /\
                    ( ValueLambda "$0" $ ExprValue $ ValueLambda "$1" $
                        ExprConstructor "Add" [ ExprIdentifier "$0", ExprIdentifier "$1" ]
                    )
                ]
            )
    it "parses data declaration with two constructor" do
      parse_declaration "data Command = Noop | Quit"
        # shouldEqual
            ( DeclarationData "Command"
                [ "Noop" /\ ValueConstructor "Noop" []
                , "Quit" /\ ValueConstructor "Quit" []
                ]
            )
    it "parses value declaration with no parameter" do
      let
        expression = ExprValue (ValueInt 42)
        declaration = DeclarationValue "x" expression
      parse_declaration "x = 42" # shouldEqual declaration
    it "parses value declaration with one parameter" do
      let
        expression = ExprValue (ValueLambda "x" (ExprValue (ValueInt 42)))
        declaration = DeclarationValue "f" expression
      parse_declaration "f x = 42" # shouldEqual declaration
    it "parses value declaration with two parameter" do
      let
        expression = ExprValue
          ( ValueLambda "x"
              (ExprValue (ValueLambda "y" (ExprValue (ValueInt 42))))
          )
        declaration = DeclarationValue "f" expression
      parse_declaration "f x y = 42" # shouldEqual declaration
  describe "expression parser" do
    let
      f = ExprIdentifier "f"
      x = ExprIdentifier "x"
      y = ExprIdentifier "y"
    it "parses identifiers" do
      parse_expression "x" # shouldEqual x
    it "parses lambda" do
      parse_expression "\\x -> x" # shouldEqual (ExprValue (ValueLambda "x" x))
    it "parses app" do
      parse_expression "f x" # shouldEqual (ExprApp f x)
    it "parses chained app" do
      parse_expression "f x y" # shouldEqual (ExprApp (ExprApp f x) y)
    it "parses let expression" do
      parse_expression """let x = 1 in x """
        # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) x)
      parse_expression """let x = 1 in f x """
            # shouldEqual (ExprLet (Map.singleton "x" $ ExprValue $ ValueInt 1) (ExprApp f x))

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
        f = ValueLambda "x" (ExprIdentifier "x")
        ast = parse_expression "f 42"
        env = Map.singleton "f" f
      evaluate_expr env ast # shouldEqual (ValueInt 42)
    it "applyes Constructor" do
      let
        ast = parse_expression "Foo 42"
        env = Map.singleton "Foo" (ValueLambda "$0" (ExprConstructor "Foo" [ ExprIdentifier "$0" ]))
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
