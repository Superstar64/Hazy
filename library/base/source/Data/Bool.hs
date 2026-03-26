module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

import Hazy.Prelude (Bool (..), not, otherwise, placeholder, (&&), (||))
import Prelude (error)

bool :: a -> a -> Bool -> a
bool = placeholder
