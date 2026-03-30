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
import Hazy.Prelude (Either (..), either, placeholder)
import Prelude ()

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
