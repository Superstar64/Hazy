{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This module contains Hazy's Prelude variant. As of now, this is largely
-- subset of the proper Prelude that is needed by helper code.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Prelude
  ( module Hazy.Prelude,
    module Hazy.Builtin,
    error,
    pack,
    putStrLn,
    trace,
  )
where

import Hazy
import Hazy.Builtin

infixr 9 .

infixr 5 ++

infixl 4 <$>

infixr 3 &&

infixr 2 ||

infixr 0 $

class Bounded a where
  minBound :: a
  maxBound :: a

instance Bounded Int where
  minBound = primIntMinBound
  maxBound = primIntMaxBound

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

($) :: (a -> b) -> a -> b
($) = id

(&&), (||) :: Bool -> Bool -> Bool
True && x = x
False && _ = False
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

otherwise :: Bool
otherwise = True

type String = [Char]

data RealWorld

newtype IO a = IO {runIO :: ST RealWorld a}

instance Functor IO where
  fmap = liftM

instance Applicative IO where
  pure x = IO (pure x)
  (<*>) = ap

instance Monad IO where
  IO m >>= f = IO (m >>= (runIO . f))

undefined :: a
undefined = error (pack "Prelude.undefined")

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

placeholder :: a
placeholder = error (pack "Prelude.placeholder")

data Text

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f a = do
  a' <- a
  return (f a')

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')
