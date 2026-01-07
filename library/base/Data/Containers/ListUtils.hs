module Data.Containers.ListUtils
  ( nubOrd,
    nubOrdOn,
    nubInt,
    nubIntOn,
  )
where

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = error "todo"

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn = error "todo"

nubInt :: [Int] -> [Int]
nubInt = error "todo"

nubIntOn :: (a -> Int) -> [a] -> [a]
nubIntOn = error "todo"
