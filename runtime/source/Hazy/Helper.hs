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
  where
    not False = True
    not True = False
