module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

import Hazy.Prelude (Bool (..), not, otherwise, placeholder)
import Prelude (error)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
(&&) = placeholder

infixr 2 ||

(||) :: Bool -> Bool -> Bool
(||) = placeholder

bool :: a -> a -> Bool -> a
bool = placeholder
