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
import Hazy.Helper (ratio, reduce)

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

class (Real a, Fractional a) => RealFrac a where
  properFraction :: (Integral b) => a -> (b, a)
  truncate, round :: (Integral b) => a -> b
  ceiling, floor :: (Integral b) => a -> b

  truncate x = m where (m, _) = properFraction x

  round x =
    let (n, r) = properFraction x
        m = if r < 0 then n - 1 else n + 1
     in case signum (abs r - 0.5) of
          -1 -> n
          0 -> if even n then n else m
          1 -> m

  ceiling x = if r > 0 then n + 1 else n
    where
      (n, r) = properFraction x

  floor x = if r < 0 then n - 1 else n
    where
      (n, r) = properFraction x

instance (Integral a) => RealFrac (Ratio a) where
  properFraction (x :% y) = (fromIntegral q, r :% y)
    where
      (q, r) = quotRem x y

subtract :: (Num a) => a -> a -> a
subtract = flip (-)

even, odd :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0
odd = not . even

gcd :: (Integral a) => a -> a -> a
gcd 0 0 = error $ pack "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' x 0 = x
    gcd' x y = gcd' y (x `rem` y)

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

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

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

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

(%) :: (Integral a) => a -> a -> Ratio a
numerator, denominator :: (Integral a) => Ratio a -> a
x % y = ratio (reduce (x * signum y) (abs y))

type Rational = Ratio Integer

numerator (x :% _) = x

denominator (_ :% y) = y
