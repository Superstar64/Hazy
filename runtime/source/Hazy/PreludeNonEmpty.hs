module Hazy.PreludeNonEmpty where

data NonEmpty a = a :| [a]

infixr 5 :|
