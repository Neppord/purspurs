module Test.RPython.Main where

import Prelude

import Data.Array (intercalate)
import Data.Array.NonEmpty.Internal (NonEmptyArray(NonEmptyArray))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple (Tuple(Tuple), snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)
import Data.Array (uncons) as Array
import PureScript.CST as CST
import PureScript.CST.Types as CST
import Data.String.CodeUnits (singleton) as CodeUnits

data Expr
  = IntLiteral String
  | FloatLiteral Number
  | StrLiteral String
  | BoolLiteral Boolean
  | Var String
  | Access Expr (Array String)
  | BinaryOp BinOp Expr Expr
  | UnaryOp UnOp Expr
  | IfExpr Expr Expr Expr
  | Call Expr (Array Expr)
  | Lambda (Array String) Expr
  | TupleLiteral (Array Expr)
  | ListLiteral (Array Expr)

data Stmt
  = ExprStmt Expr
  | Assign String Expr
  | Return Expr
  | FunctionDef String (Array String) (Array Stmt)
  | IfStmt Expr (Array Stmt) (Array Stmt)
  | ForStmt String Expr (Array Stmt)
  | WhileStmt Expr (Array Stmt)
  | Pass

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | And
  | Or
  | LessThan
  | GreaterThan
  | Equals
  | NotEquals
  | LessEquals
  | GreaterEquals

data UnOp
  = Negate
  | Not

instance Show Expr where
  show = case _ of
    IntLiteral n -> n
    FloatLiteral f -> show f
    StrLiteral s -> "\"" <> s <> "\""
    BoolLiteral b -> if b then "True" else "False"
    Var v -> v
    Access v path -> [ show v ] <> path # intercalate "."
    BinaryOp op left right -> "(" <> show left <> " " <> show op <> " " <> show right <> ")"
    UnaryOp op expr -> show op <> " " <> show expr
    IfExpr cond t f -> "(" <> show t <> " if " <> show cond <> " else " <> show f <> ")"
    Call f args -> show f <> "(" <> intercalate ", " (show <$> args) <> ")"
    Lambda args body -> "lambda " <> intercalate ", " args <> ": " <> show body
    TupleLiteral elems -> "(" <> intercalate ", " (show <$> elems) <> ")"
    ListLiteral elems -> "[" <> intercalate ", " (show <$> elems) <> "]"

instance Show Stmt where
  show = case _ of
    ExprStmt expr -> show expr
    Assign v expr -> v <> " = " <> show expr
    Return expr -> "return " <> show expr
    FunctionDef name args body -> "def " <> name <> "(" <> intercalate ", " args <> "):\n" <> indent (intercalate "\n" (show <$> body))
    IfStmt cond t f -> "if " <> show cond <> ":\n" <> indent (intercalate "\n" (show <$> t)) <> "\nelse:\n" <> indent (intercalate "\n" (show <$> f))
    ForStmt var iter body -> "for " <> var <> " in " <> show iter <> ":\n" <> indent (intercalate "\n" (show <$> body))
    WhileStmt cond body -> "while " <> show cond <> ":\n" <> indent (intercalate "\n" (show <$> body))
    Pass -> "pass"

instance Show BinOp where
  show = case _ of
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Modulus -> "%"
    And -> "and"
    Or -> "or"
    LessThan -> "<"
    GreaterThan -> ">"
    Equals -> "=="
    NotEquals -> "!="
    LessEquals -> "<="
    GreaterEquals -> ">="

instance Show UnOp where
  show = case _ of
    Negate -> "-"
    Not -> "not"

lines :: String -> Array String
lines = split (Pattern "\n")

unlines :: Array String -> String
unlines = intercalate "\n"

-- Helper function to indent Python code
indent :: String -> String
indent = unlines <<< map ("    " <> _) <<< lines

translate_expression :: CST.Expr Void -> Expr
translate_expression e = case e of
  CST.ExprInt _ (CST.BigInt i) -> IntLiteral i
  CST.ExprInt _ (CST.BigHex i) -> IntLiteral i
  CST.ExprInt _ (CST.SmallInt i) -> IntLiteral (show i)
  CST.ExprBoolean _ b -> BoolLiteral b
  CST.ExprString _ string -> StrLiteral string
  CST.ExprChar _ char -> StrLiteral (CodeUnits.singleton char)
  CST.ExprNumber _ n -> FloatLiteral n
  CST.ExprIdent identifier -> Var $ compile_identifier identifier
  CST.ExprConstructor constructor -> Var $ compile_constructor constructor
  CST.ExprRecordAccessor { expr, path } ->
    Access
      (translate_expression expr)
      (path # elements_from_seperated <#> compile_name_label)
  CST.ExprArray array ->
    array
      # unwrapped
      # maybe [] elements_from_seperated
      <#> translate_expression
      # ListLiteral
  CST.ExprOp left tail -> compile_operator (translate_expression left) tail
  _ -> StrLiteral "Error"

compile_operator
  :: Expr
  -> (NonEmptyArray (Tuple (CST.QualifiedName CST.Operator) (CST.Expr Void)))
  -> Expr
compile_operator left (NonEmptyArray [ right_ ]) = case right_ of
  Tuple (CST.QualifiedName { name: CST.Operator "*" }) right ->
    BinaryOp Multiply left (translate_expression right)
  Tuple (CST.QualifiedName { name: CST.Operator _ {- "+" -} }) right ->
    BinaryOp Add left (translate_expression right)
compile_operator left (NonEmptyArray more_then_one) = case Array.uncons more_then_one of
  Just { head: Tuple (CST.QualifiedName { name: CST.Operator op }) right, tail } -> case op of
    "*" -> compile_operator (BinaryOp Multiply left (translate_expression right)) (NonEmptyArray tail)
    _ -> BinaryOp Add left (translate_expression (CST.ExprOp right (NonEmptyArray tail)))
  Nothing -> left

compile_expression :: CST.Expr Void -> String
compile_expression e = case e of
  CST.ExprInt _ (CST.BigInt i) -> i
  CST.ExprInt _ (CST.BigHex i) -> i
  CST.ExprInt _ (CST.SmallInt i) -> show i
  CST.ExprBoolean _ true -> "True"
  CST.ExprBoolean _ false -> "False"
  CST.ExprString _ string -> show string
  CST.ExprChar _ char -> show char
  CST.ExprNumber _ n -> show n
  CST.ExprIdent identifier -> compile_identifier identifier
  CST.ExprConstructor constructor -> compile_constructor constructor
  CST.ExprRecordAccessor { expr, path } -> compile_expression expr <> "." <> compile_path path
  CST.ExprArray array -> compile_array array
  _ -> ""

elements_from_seperated :: forall a. CST.Separated a -> Array a
elements_from_seperated (CST.Separated { head, tail }) = [ head ] <> (tail <#> snd)

unwrapped :: forall a. CST.Wrapped a -> a
unwrapped (CST.Wrapped { value }) = value

compile_array :: CST.Wrapped (Maybe (CST.Separated (CST.Expr Void))) -> String
compile_array (CST.Wrapped { value }) =
  let
    values = value
      <#> elements_from_seperated
      # fromMaybe []
      <#> compile_expression
  in
    "[" <> intercalate ", " values <> "]"

compile_identifier :: CST.QualifiedName CST.Ident -> String
compile_identifier (CST.QualifiedName { name: CST.Ident name }) = name

compile_constructor :: CST.QualifiedName CST.Proper -> String
compile_constructor (CST.QualifiedName { name: CST.Proper name }) = name

compile_path :: CST.Separated (CST.Name CST.Label) -> String
compile_path path =
  path # elements_from_seperated <#> compile_name_label # intercalate "."

compile_name_label :: CST.Name CST.Label -> String
compile_name_label (CST.Name { name: CST.Label name }) = name

compile_expression_from_string :: String -> String
compile_expression_from_string str = case CST.parseExpr str of
  CST.ParseSucceeded expr -> expr
    # translate_expression
    # show
  _ -> ""

compile_module :: String -> String
compile_module source = case CST.parseModule source of
  CST.ParseSucceeded _ ->
    """from __future__ import print_function
def main():
    print("hello world")


def entry_point(*args):
    main()
    return 0


def target(*args):
    return entry_point, None
"""
  _ -> ""

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter ] do
  describe "rpython compiler" do
    it "compiles simple program" do
      compile_module
        """module Main where
main = print "hello world"
        """ # shouldEqual
        """from __future__ import print_function
def main():
    print("hello world")


def entry_point(*args):
    main()
    return 0


def target(*args):
    return entry_point, None
"""
    it "compiles values" do
      compile_expression_from_string "1" # shouldEqual "1"
      compile_expression_from_string "true" # shouldEqual "True"
      compile_expression_from_string "4.2" # shouldEqual "4.2"
      compile_expression_from_string "foo" # shouldEqual "foo"
      compile_expression_from_string "Foo" # shouldEqual "Foo"
      compile_expression_from_string "'a'" # shouldEqual "\"a\""
      compile_expression_from_string "\"a\"" # shouldEqual "\"a\""
      compile_expression_from_string "bar.foo" # shouldEqual "bar.foo"
      compile_expression_from_string "bar.foo.baz" # shouldEqual "bar.foo.baz"
      compile_expression_from_string "[]" # shouldEqual "[]"
      compile_expression_from_string "[1]" # shouldEqual "[1]"
      compile_expression_from_string "[1, 2]" # shouldEqual "[1, 2]"

    it "compiles expressions" do
      compile_expression_from_string "1 + 2" # shouldEqual "(1 + 2)"
      compile_expression_from_string "1 + 2 * 3" # shouldEqual "(1 + (2 * 3))"
      compile_expression_from_string "1 * 2 + 3" # shouldEqual "((1 * 2) + 3)"
      compile_expression_from_string "\"Hello \" <> \"World\"" # shouldEqual "(\"Hello \" + \"World\")"
