module Data.Containers.ListUtils
  ( nubOrd,
    nubOrdOn,
    nubInt,
    nubIntOn,
  )
where

import Hazy.Prelude (placeholder)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = placeholder

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn = placeholder

nubInt :: [Int] -> [Int]
nubInt = placeholder

nubIntOn :: (a -> Int) -> [a] -> [a]
nubIntOn = placeholder
