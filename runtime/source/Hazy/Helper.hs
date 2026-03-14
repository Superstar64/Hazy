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

eqBoolEqual :: Bool -> Bool -> Bool
eqBoolEqual False False = True
eqBoolEqual True True = True
eqBoolEqual _ _ = False

eqBoolNotEqual :: Bool -> Bool -> Bool
eqBoolNotEqual a b = not (eqBoolEqual a b)
