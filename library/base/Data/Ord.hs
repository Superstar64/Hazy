{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Ord
  ( Ord (..),
    Ordering (..),
    Down (..),
    comparing,
    clamp,
  )
where

import Data.Bool (Bool)
import Prelude (Eq, error)

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  infix 4 <, <=, >=, >
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

data Ordering
  = LT
  | EQ
  | GT

newtype Down a = Down
  { getDown :: a
  }

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing = error "todo"

clamp :: (Ord a) => (a, a) -> a -> a
clamp = error "todo"
