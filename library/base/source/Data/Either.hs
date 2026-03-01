module Data.Either
  ( Either (..),
    either,
    lefts,
    rights,
    isLeft,
    isRight,
    fromLeft,
    fromRight,
    partitionEithers,
  )
where

import Data.Bool (Bool)
import Hazy (placeholder)
import Prelude (error)

data Either a b
  = Left a
  | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = placeholder

lefts :: [Either a b] -> [a]
lefts = placeholder

rights :: [Either a b] -> [b]
rights = placeholder

isLeft :: Either a b -> Bool
isLeft = placeholder

isRight :: Either a b -> Bool
isRight = placeholder

fromLeft :: a -> Either a b -> a
fromLeft = placeholder

fromRight :: b -> Either a b -> b
fromRight = placeholder

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = placeholder
