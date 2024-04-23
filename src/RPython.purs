module RPython where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(Just))
import Data.String.Common (replace, toLower)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (argv)
import RPython.Module (compile_module)

main :: Effect Unit
main = do
  names <- argv
  case names !! 2 of
    Just name -> compile_file name
    _ -> log ("Error: cant compile" <> show names)

compile_file :: String -> Effect Unit
compile_file file_name = do
  text <- readTextFile UTF8 file_name
  let
    python = compile_module text
    python_name = file_name
      # replace (Pattern ".purs") (Replacement ".py")
      # toLower
  writeTextFile UTF8 python_name python