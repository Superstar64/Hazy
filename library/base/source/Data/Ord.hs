module Data.Ord
  ( Ord (..),
    Ordering (..),
    Down (..),
    comparing,
    clamp,
  )
where

import Hazy.Prelude (Ord (..), Ordering (..), placeholder)
import Prelude (Eq, error)

newtype Down a = Down
  { getDown :: a
  }

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing = placeholder

clamp :: (Ord a) => (a, a) -> a -> a
clamp = placeholder
