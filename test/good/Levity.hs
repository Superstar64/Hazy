{-# LANGUAGE_HAZY NoStableImports #-}
module Levity where

import Hazy.Prelude
import Prelude ()

type List :: Levity -> Type -> Type
data List s a

type StrictList = List Strict

type LazyList = List Lazy
