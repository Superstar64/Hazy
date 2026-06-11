module Data.Bool
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    bool,
  )
where

import Hazy.Prelude (placeholder)

bool :: a -> a -> Bool -> a
bool = placeholder
