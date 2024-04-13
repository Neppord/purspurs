module PursPurs.Operator where

import PursPurs.Fixity (Fixity)


type Operator value =
  { operation :: value
  , precedence :: Int
  , fixity :: Fixity
  }