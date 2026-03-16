{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This module contains helpers that the runtime call into. This is mainly
-- and instance methods and type class defaults that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy

defaultEqual :: (Eq a) => a -> a -> Bool
defaultEqual x y = not (x /= y)

defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultNotEqual x y = not (x == y)

defaultSucc :: (Enum a) => a -> a
defaultSucc = toEnum . (\x -> x + 1) . fromEnum

defaultPred :: (Enum a) => a -> a
defaultPred = toEnum . (subtract 1) . fromEnum

defaultToEnum :: (Enum a) => Int -> a
defaultToEnum = undefined

defaultFromEnum :: (Enum a) => a -> Int
defaultFromEnum = undefined

defaultEnumFrom :: (Enum a) => a -> [a]
defaultEnumFrom x = map toEnum [fromEnum x ..]

defaultEnumFromTo :: (Enum a) => a -> a -> [a]
defaultEnumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

defaultEnumFromThen :: (Enum a) => a -> a -> [a]
defaultEnumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

defaultEnumFromThenTo :: (Enum a) => a -> a -> a -> [a]
defaultEnumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

defaultPlus :: (Num a) => a -> a -> a
defaultPlus = undefined

defaultMinus :: (Num a) => a -> a -> a
defaultMinus x y = x + negate y

defaultMultiply :: (Num a) => a -> a -> a
defaultMultiply = undefined

defaultNegate :: (Num a) => a -> a
defaultNegate x = 0 - x

defaultAbs :: (Num a) => a -> a
defaultAbs = undefined

defaultSignum :: (Num a) => a -> a
defaultSignum = undefined

defaultFromInteger :: (Num a) => Integer -> a
defaultFromInteger = undefined

defaultFmap :: (Functor f) => (a -> b) -> f a -> f b
defaultFmap = undefined

defaultFconst :: (Functor f) => a -> f b -> f a
defaultFconst a = fmap (const a)

defaultPure :: (Applicative f) => a -> f a
defaultPure = undefined

defaultAp :: (Applicative f) => f (a -> b) -> f a -> f b
defaultAp = liftA2 id

defaultLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
defaultLiftA2 f a b = f <$> a <*> b

defaultDiscardLeft :: (Applicative f) => f a -> f b -> f b
defaultDiscardLeft = liftA2 (flip const)

defaultDiscardRight :: (Applicative f) => f a -> f b -> f a
defaultDiscardRight = liftA2 const

defaultBind :: (Monad m) => m a -> (a -> m b) -> m b
defaultBind = undefined

defaultThen :: (Monad m) => m a -> m b -> m b
defaultThen a b = a >>= \_ -> b

defaultReturn :: (Monad m) => a -> m a
defaultReturn = pure

defaultFail :: (MonadFail m) => String -> m a
defaultFail = undefined

eqBoolEqual :: Bool -> Bool -> Bool
eqBoolEqual False False = True
eqBoolEqual True True = True
eqBoolEqual _ _ = False

eqBoolNotEqual :: Bool -> Bool -> Bool
eqBoolNotEqual a b = not (eqBoolEqual a b)

enumIntSucc :: Int -> Int
enumIntSucc x = x + 1

enumIntPred :: Int -> Int
enumIntPred x = x - 1

enumIntToEnum :: Int -> Int
enumIntToEnum = id

enumIntFromEnum :: Int -> Int
enumIntFromEnum = id

enumIntEnumFrom :: Int -> [Int]
enumIntEnumFrom x = [x .. maxBound]

enumIntEnumFromTo :: Int -> Int -> [Int]
enumIntEnumFromTo x y = [x, x + 1 .. y]

enumIntEnumFromThen :: Int -> Int -> [Int]
enumIntEnumFromThen x y = [x, y .. maxBound]

enumIntEnumFromThenTo :: Int -> Int -> Int -> [Int]
enumIntEnumFromThenTo from thenx to = run from
  where
    run from | from > to = []
    run from = from : run (from + step)
    step = thenx - from

enumIntegerSucc :: Integer -> Integer
enumIntegerSucc x = x + 1

enumIntegerPred :: Integer -> Integer
enumIntegerPred x = x - 1

enumIntegerEnumFrom :: Integer -> [Integer]
enumIntegerEnumFrom x = [x, x + 1 ..]

enumIntegerEnumFromTo :: Integer -> Integer -> [Integer]
enumIntegerEnumFromTo x y = [x, x + 1 .. y]

enumIntegerEnumFromThen :: Integer -> Integer -> [Integer]
enumIntegerEnumFromThen from thenx = run from
  where
    run from = from : run (from + step)
    step = thenx - from

enumIntegerEnumFromThenTo :: Integer -> Integer -> Integer -> [Integer]
enumIntegerEnumFromThenTo from thenx to = run from
  where
    run from | from > to = []
    run from = from : run (from + step)
    step = thenx - from

enumBoolSucc :: Bool -> Bool
enumBoolSucc = defaultSucc

enumBoolPred :: Bool -> Bool
enumBoolPred = defaultPred

enumBoolToEnum :: Int -> Bool
enumBoolToEnum = \case
  0 -> False
  1 -> True

enumBoolFromEnum :: Bool -> Int
enumBoolFromEnum = \case
  False -> 0
  True -> 1

enumBoolEnumFrom :: Bool -> [Bool]
enumBoolEnumFrom = defaultEnumFrom

enumBoolEnumFromThen :: Bool -> Bool -> [Bool]
enumBoolEnumFromThen = defaultEnumFromThen

enumBoolEnumFromTo :: Bool -> Bool -> [Bool]
enumBoolEnumFromTo = defaultEnumFromTo

enumBoolEnumFromThenTo :: Bool -> Bool -> Bool -> [Bool]
enumBoolEnumFromThenTo = defaultEnumFromThenTo
