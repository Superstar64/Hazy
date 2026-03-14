{-# LANGUAGE_HAZY NoStableImports #-}
module Hazy.Helper where

import Hazy
import Prelude ()

eqBoolEqual :: Bool -> Bool -> Bool
eqBoolEqual False False = True
eqBoolEqual True True = True
eqBoolEqual _ _ = False

eqBoolNotEqual :: Bool -> Bool -> Bool
eqBoolNotEqual a b = not (eqBoolEqual a b)

defaultEqual :: (Eq a) => a -> a -> Bool
defaultEqual x y = not (x /= y)

defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultNotEqual x y = not (x == y)

not False = True
not True = False
