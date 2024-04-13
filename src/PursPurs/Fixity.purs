module PursPurs.Fixity where

import Prelude

data Fixity = Infixl | Infixr | Infix

instance showFixity :: Show Fixity where
    show Infixl = "infixl"
    show Infixr = "infixr"
    show Infix = "infix"

instance Eq Fixity where
    eq Infixl Infixl = true
    eq Infixr Infixr = true
    eq Infix Infix = true
    eq _ _ = false
