{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This module contains Hazy's Prelude variant. As of now, this is largely
-- subset of the proper Prelude that is needed by helper code.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Prelude where

import Hazy

infixr 9 .

infixl 4 <$>

infix 4 <, <=, >=, >

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a

  compare x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT

  x <= y = compare x y /= GT
  x < y = compare x y == LT
  x >= y = compare x y /= LT
  x > y = compare x y == GT

  max x y
    | x <= y = y
    | otherwise = x
  min x y
    | x <= y = x
    | otherwise = y

instance Ord Int where
  (<=) = intLessThenEqual

class Bounded a where
  minBound :: a
  maxBound :: a

instance Bounded Int where
  minBound = boundedIntMinBound
  maxBound = boundedIntMaxBound

subtract :: (Num a) => a -> a -> a
subtract = flip (-)

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

not :: Bool -> Bool
not True = False
not False = True

otherwise :: Bool
otherwise = True

type String = [Char]

data IO a

data Ordering = LT | EQ | GT

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == EQ = True
  _ == _ = False

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

data Text

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
