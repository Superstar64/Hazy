module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

import Hazy (Bool (..), placeholder)
import Prelude (error)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
(&&) = placeholder

infixr 2 ||

(||) :: Bool -> Bool -> Bool
(||) = placeholder

not :: Bool -> Bool
not = placeholder

otherwise :: Bool
otherwise = placeholder

bool :: a -> a -> Bool -> a
bool = placeholder
