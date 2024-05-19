module Test.Interpreter.E2E where

import Prelude

import Data.Maybe (Maybe, fromMaybe, maybe)
import Effect.Aff (Aff)
import Interpreter (default_env, evaluate_import, evaluate_in_memory, evaluate_module, print)
import Parser (parse_module)
import PursPurs.Expression (Expr)
import PursPurs.Import (Import(Import))
import PursPurs.Load (load_source)
import PursPurs.Value (Scope, empty_scope, lookup_value)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Assertions.String (shouldStartWith)
import Data.Array (foldl) as Array

run_program :: Array String -> String
run_program program = Array.foldl (\env decl -> evaluate_in_memory decl env) default_env program
  # print

load_module :: String -> Aff (Maybe (Scope Expr))
load_module program = evaluate_module
  { module_loader: load_source }
  (parse_module program)

run_module :: String -> Aff String
run_module program = do
  result <- load_module program
  result
    <#> (\scope -> lookup_value "main" scope)
    # maybe "error" show
    # pure

spec :: Spec Unit
spec = describe "End to End tests" do
  describe "loading source from github" do
    it "should be able to grabe the source" do
      source <- load_source "Data.Boolean"
        <#> fromMaybe "Nothing"
      shouldStartWith source "module Data.Boolean"

    it "should be able to evaluate the source" do
      scope <- evaluate_import { module_loader: load_source } (Import "Data.Boolean")
      scope # shouldNotEqual empty_scope
  describe "evaluate module" do
    it "evaluates main" do
      output <- run_module "module Main where\nmain = 1 "
      output # shouldEqual "1"
    it "imports otherwise from Data.Boolean" do
      output <- run_module "module Main where\nimport Data.Boolean\nmain = otherwise"
      output # shouldEqual "true"
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
    it "handles session case of" do
      run_program
        [ """f x = case x of
        false -> true
        x -> x
      """
        , "f true"
        ] # shouldEqual "true"
    it "handles session case of _" do
      run_program
        [ """case 1 of
        _ -> true
      """
        ] # shouldEqual "true"
    it "handles session case of constructor without subbinders" do
      run_program
        [ "data Foo = A | B"
        , """case B of
        A -> false
        B -> true
      """
        ] # shouldEqual "true"

    it "handles session case of constructor with subbinders" do
      run_program
        [ "data Foo = A | B Boolean"
        , """case B true of
        A -> false
        B b -> b
      """
        ] # shouldEqual "true"
    it "handles 1 + 2" do
      run_program [ "1 + 2" ] # shouldEqual "3"
    it "handles 1 + 2 + 3" do
      run_program [ "1 + 2 + 3" ] # shouldEqual "6"
    it "handles 1 + 2 - 3" do
      run_program [ "1 + 2 - 3" ] # shouldEqual "0"
