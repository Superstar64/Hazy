{-# LANGUAGE_HAZY NoStableImports #-}

-- |
-- This module contains helpers that the runtime call into. This is mainly
-- and instance methods and type class defaults that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy.Prelude
import Prelude ()

defaultEqual :: (Eq a) => a -> a -> Bool
defaultEqual x y = not (x /= y)

defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultNotEqual x y = not (x == y)

defaultSucc :: (Enum a) => a -> a
defaultSucc = toEnum . (\x -> x + 1) . fromEnum

defaultPred :: (Enum a) => a -> a
defaultPred = toEnum . (subtract 1) . fromEnum

defaultEnumFrom :: (Enum a) => a -> [a]
defaultEnumFrom x = map toEnum [fromEnum x ..]

defaultEnumFromTo :: (Enum a) => a -> a -> [a]
defaultEnumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

defaultEnumFromThen :: (Enum a) => a -> a -> [a]
defaultEnumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

defaultEnumFromThenTo :: (Enum a) => a -> a -> a -> [a]
defaultEnumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

defaultMinus :: (Num a) => a -> a -> a
defaultMinus x y = x + negate y

defaultNegate :: (Num a) => a -> a
defaultNegate x = 0 - x

defaultFconst :: (Functor f) => a -> f b -> f a
defaultFconst a = fmap (const a)

defaultAp :: (Applicative f) => f (a -> b) -> f a -> f b
defaultAp = liftA2 id

defaultLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
defaultLiftA2 f a b = f <$> a <*> b

defaultDiscardLeft :: (Applicative f) => f a -> f b -> f b
defaultDiscardLeft = liftA2 (flip const)

defaultDiscardRight :: (Applicative f) => f a -> f b -> f a
defaultDiscardRight = liftA2 const

defaultThen :: (Monad m) => m a -> m b -> m b
defaultThen a b = a >>= \_ -> b

defaultReturn :: (Monad m) => a -> m a
defaultReturn = pure

eqBoolEqual :: Bool -> Bool -> Bool
eqBoolEqual False False = True
eqBoolEqual True True = True
eqBoolEqual _ _ = False

eqBoolNotEqual :: Bool -> Bool -> Bool
eqBoolNotEqual a b = not (eqBoolEqual a b)
