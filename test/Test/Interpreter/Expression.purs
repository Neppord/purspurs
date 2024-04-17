module Test.Interpreter.Expression where

import Prelude

import Interpreter (evaluate_expr)
import Parser (parse_expression)
import PursPurs.Expression (Expr(..))
import PursPurs.Value (Callable(..), Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import PursPurs.Value (empty_env, insert) as Value

simple_eval :: String -> Value Expr
simple_eval expr = evaluate_expr Value.empty_env (parse_expression expr)

spec :: Spec Unit
spec = describe "expression interptreter" do
  literals
  foreign_
  it "handle identifiers" do
    let
      x = ValueInt 42
      ast = parse_expression "x"
      env = Value.empty_env # Value.insert "x" x
    evaluate_expr env ast # shouldEqual x
  it "handle parenthesis" do
    let
      ast = parse_expression "(42)"
      env = Value.empty_env
    evaluate_expr env ast # shouldEqual (ValueInt 42)
  it "applyes functions" do
    let
      f = ValueCallable $ CallableLambda "x" Value.empty_env $ ExprIdentifier "x"
      ast = parse_expression "f 42"
      env = Value.empty_env # Value.insert "f" f
    evaluate_expr env ast # shouldEqual (ValueInt 42)
  it "applyes Constructor" do
    let
      ast = parse_expression "Foo 42"
      env = Value.empty_env #
        ( Value.insert "Foo"
            $ ValueCallable
            $ CallableLambda "$0" Value.empty_env
            $ ExprConstructor "Foo" [ ExprIdentifier "$0" ]
        )
    evaluate_expr env ast # shouldEqual (ValueConstructor "Foo" [ ValueInt 42 ])
  it "handle if-else is true" do
    let
      ast = parse_expression "if true then 1 else 2"
      env = Value.empty_env
    evaluate_expr env ast # shouldEqual (ValueInt 1)
  it "handle if-else is false" do
    let
      ast = parse_expression "if false then 1 else 2"
      env = Value.empty_env
    evaluate_expr env ast # shouldEqual (ValueInt 2)

foreign_ :: Spec Unit
foreign_ = describe "handle foregin" do
  it "calls foregin functions" do
    let
      inc = ValueCallable $ CallableForeignFn case _ of
        ValueInt i -> ValueInt (1 + i)
        _ -> ValueError "Expected Int"
    evaluate_expr (Value.empty_env # Value.insert "inc" inc) (parse_expression "inc 42")
      # shouldEqual (ValueInt 43)

literals :: Spec Unit
literals = describe "handle literals" do
  it "handles Boolean" do
    simple_eval "true" # shouldEqual (ValueBoolean true)
  it "handles Char" do
    simple_eval "'*'" # shouldEqual (ValueChar '*')
  it "handles Number" do
    simple_eval "42.0" # shouldEqual (ValueNumber 42.0)
  it "handles Int" do
    simple_eval "1" # shouldEqual (ValueInt 1)
  it "handles String" do
    simple_eval "\"Hello World!\"" # shouldEqual (ValueString "Hello World!")
  it "handles Array" do
    simple_eval "['*']" # shouldEqual (ValueArray [ ValueChar '*' ])
