module PursPurs.Operator where

import PursPurs.Fixity (Fixity)


type Operator expr =
  { operation :: expr
  , precedence :: Int
  , fixity :: Fixity
  }