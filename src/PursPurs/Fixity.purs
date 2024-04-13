module PursPurs.Fixity where

import Prelude

data Fixity = Infixl

instance showFixity :: Show Fixity where
    show Infixl = "infixl"

instance Eq Fixity where
    eq Infixl Infixl = true
    -- eq _ _ = false
