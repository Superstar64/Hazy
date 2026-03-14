{-# LANGUAGE_HAZY NoStableImports #-}

-- |
-- This module contains the subset of the Prelude needed by the helper code.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Prelude (module Hazy, module Hazy.Prelude) where

import Hazy
import Prelude ()

infixr 9 .

infixl 4 <$>

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

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
