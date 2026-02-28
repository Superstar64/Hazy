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
import Prelude (error)

data Either a b
  = Left a
  | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = error "todo"

lefts :: [Either a b] -> [a]
lefts = error "todo"

rights :: [Either a b] -> [b]
rights = error "todo"

isLeft :: Either a b -> Bool
isLeft = error "todo"

isRight :: Either a b -> Bool
isRight = error "todo"

fromLeft :: a -> Either a b -> a
fromLeft = error "todo"

fromRight :: b -> Either a b -> b
fromRight = error "todo"

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = error "todo"
