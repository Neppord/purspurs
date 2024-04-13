module Test.RPython.Main where

import Prelude

import Data.Array (intercalate)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import PureScript.CST as CST
import PureScript.CST.Types as CST

compile_expression :: CST.Expr Void -> String
compile_expression e = case e of
  CST.ExprInt _ (CST.BigInt i) -> i
  CST.ExprInt _ (CST.BigHex i) -> i
  CST.ExprInt _ (CST.SmallInt i) -> show i
  CST.ExprBoolean _ true -> "True"
  CST.ExprBoolean _ false -> "False"
  CST.ExprNumber _ n -> show n
  CST.ExprIdent (CST.QualifiedName { name: CST.Ident name }) -> name
  CST.ExprRecordAccessor { expr, path } -> compile_expression expr <> "." <> compile_path path
  _ -> ""

compile_path :: CST.Separated (CST.Name CST.Label) -> String
compile_path (CST.Separated { head, tail }) =
  let
    all = [ head ] <> (tail <#> snd)
  in
    all
      <#> compile_name_label
      # intercalate "."

compile_name_label :: CST.Name CST.Label -> String
compile_name_label (CST.Name { name: CST.Label name }) = name

compile_expression_from_string :: String -> String
compile_expression_from_string str = case CST.parseExpr str of
  CST.ParseSucceeded expr -> compile_expression expr
  _ -> ""

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  describe "rpython compiler" do
    it "compiles values" do
      compile_expression_from_string "1" # shouldEqual "1"
      compile_expression_from_string "true" # shouldEqual "True"
      compile_expression_from_string "4.2" # shouldEqual "4.2"
      compile_expression_from_string "foo" # shouldEqual "foo"
      compile_expression_from_string "bar.foo" # shouldEqual "bar.foo"
      compile_expression_from_string "bar.foo.baz" # shouldEqual "bar.foo.baz"

