module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

import Hazy (Bool (..))
import Prelude (error)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
(&&) = error "todo"

infixr 2 ||

(||) :: Bool -> Bool -> Bool
(||) = error "todo"

not :: Bool -> Bool
not = error "todo"

otherwise :: Bool
otherwise = error "todo"

bool :: a -> a -> Bool -> a
bool = error "todo"
