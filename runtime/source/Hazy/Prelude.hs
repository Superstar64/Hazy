-- |
-- This module contains Hazy's Prelude variant. This is a superset of Haskell's
-- proper Prelude. The definitions here are largely taken from the Haskell2010
-- report.
module Hazy.Prelude
  ( module Hazy.Prelude,
    module Hazy.PreludeList,
    module Hazy.PreludeText,
    module Hazy.PreludeIO,
    module Hazy.PreludeChar,
    module Hazy.PreludeRatio,
    module Hazy.PreludeNumeric,
    module Hazy.PreludeMonad,
    module Hazy.PreludeString,
    module Hazy.PreludeMonoid,
    module Hazy.PreludeNonEmpty,
    module Hazy.Builtin,
    errorText,
    pack,
    unpack,
    putStrLnText,
    traceText,
    generalCategory,
  )
where

import Hazy
import Hazy.Builtin
import Hazy.Helper (Strict (..), enumCompare, enumEqual, ratio, reduce)
import Hazy.PreludeChar
import Hazy.PreludeIO
import Hazy.PreludeList
import Hazy.PreludeMonad
import Hazy.PreludeMonoid
import Hazy.PreludeNonEmpty
import Hazy.PreludeNumeric
import Hazy.PreludeRatio
import Hazy.PreludeString
import Hazy.PreludeText

infixr 9 .

infixr 8 ^, ^^, **

infixr 3 &&

infixr 2 ||

infixr 1 =<<

infixr 0 $, $!, `seq`

class Bounded a where
  minBound :: a
  maxBound :: a

class (Fractional a) => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  (**), logBase :: a -> a -> a
  sin, cos, tan :: a -> a
  asin, acos, atan :: a -> a
  sinh, cosh, tanh :: a -> a
  asinh, acosh, atanh :: a -> a

  x ** y = exp (log x * y)
  logBase x y = log y / log x
  sqrt x = x ** 0.5
  tan x = sin x / cos x
  tanh x = sinh x / cosh x

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

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN,
    isInfinite,
    isDenormalized,
    isNegativeZero,
    isIEEE ::
      a -> Bool
  atan2 :: a -> a -> a

  exponent x = if m == 0 then 0 else n + floatDigits x
    where
      (m, n) = decodeFloat x

  significand x = encodeFloat m (-floatDigits x)
    where
      (m, _) = decodeFloat x

  scaleFloat k x = encodeFloat m (n + k)
    where
      (m, n) = decodeFloat x

  atan2 y x
    | x > 0 = atan (y / x)
    | x == 0 && y > 0 = pi / 2
    | x < 0 && y > 0 = pi + atan (y / x)
    | (x <= 0 && y < 0)
        || (x < 0 && isNegativeZero y)
        || (isNegativeZero x && isNegativeZero y) =
        -(atan2 (-y) x)
    | y == 0 && (x < 0 || isNegativeZero x) =
        pi
    | x == 0 && y == 0 = y
    | otherwise = x + y

subtract :: (Num a) => a -> a -> a
subtract = flip (-)

even, odd :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0
odd = not . even

gcd :: (Integral a) => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' x 0 = x
    gcd' x y = gcd' y (x `rem` y)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` (gcd x y)) * y)

(^) :: (Num a, Integral b) => a -> b -> a
x ^ 0 = 1
x ^ n | n > 0 = f x (n - 1) x
  where
    f _ 0 y = y
    f x n y = g x n
      where
        g x n
          | even n = g (x * x) (n `quot` 2)
          | otherwise = f x (n - 1) (x * y)
_ ^ _ = error "Prelude.^: negative exponent"

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x ^ n else recip (x ^ (-n))

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

class (Functor t, Foldable t) => Traversable t where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  sequenceA :: (Applicative f) => t (f a) -> f (t a)
  sequenceA = traverse id
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  mapM = traverse
  sequence :: (Monad m) => t (m a) -> m (t a)
  sequence = sequenceA

instance Traversable [] where
  traverse f = \case
    [] -> pure []
    (x : xs) -> (:) <$> f x <*> traverse f xs

  mapM f as = sequence (map f as)
  sequence = foldr mcons (return [])
    where
      mcons p q = p >>= \x -> q >>= \y -> return (x : y)

instance Traversable NonEmpty where
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = sequence_ . map f . toList

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< x = x >>= f

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

seq :: a -> b -> b
seq a b = case Strict a of Strict _ -> b

($), ($!) :: (a -> b) -> a -> b
($) = id
f $! x = x `seq` f x

instance Bounded Bool where
  minBound = False
  maxBound = True

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

instance Bounded Char where
  minBound = '\0'
  maxBound = '\1114111'

type String = [Char]

data Maybe a = Nothing | Just a

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just a == Just b = a == b
  _ == _ = False

instance (Ord a) => Ord (Maybe a) where
  Nothing `compare` Nothing = EQ
  Nothing `compare` Just _ = LT
  Just _ `compare` Nothing = GT
  Just a `compare` Just b = a `compare` b

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  (<*>) = ap

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing >>= k = Nothing

instance MonadFail Maybe where
  fail s = Nothing

instance Foldable Maybe where
  foldr f z = foldr f z . toList
  toList = \case
    Nothing -> []
    Just x -> [x]

instance Traversable Maybe where
  traverse f = \case
    Nothing -> pure Nothing
    Just a -> Just <$> f a

data Either a b = Left a | Right b

instance (Eq a, Eq b) => Eq (Either a b) where
  Left a == Left b = a == b
  Right a == Right b = a == b
  _ == _ = False

instance (Ord a, Ord b) => Ord (Either a b) where
  Left a `compare` Left b = a `compare` b
  Left _ `compare` Right _ = LT
  Right _ `compare` Left _ = GT
  Right a `compare` Right b = a `compare` b

instance Functor (Either a) where
  fmap = liftM

instance Applicative (Either a) where
  (<*>) = ap

instance Monad (Either a) where
  Left a >>= _ = Left a
  Right a >>= f = f a

instance Foldable (Either a) where
  foldr f z = foldr f z . toList
  toList = \case
    Left {} -> []
    Right x -> [x]

instance Traversable (Either a) where
  traverse f = \case
    Left a -> pure (Left a)
    Right a -> Right <$> f a

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

data RealWorld

newtype IO a = IO {runIO :: ST RealWorld a}

instance Functor IO where
  fmap = liftM

instance Applicative IO where
  pure x = IO (pure x)
  (<*>) = ap

instance Monad IO where
  IO m >>= f = IO (m >>= (runIO . f))

instance MonadFail IO where
  fail s = ioError (userError s)

instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance Bounded Int where
  minBound = primIntMinBound
  maxBound = primIntMaxBound

data Float

instance Eq Float

instance Ord Float

instance Num Float

instance Real Float

instance Fractional Float

instance Floating Float

instance RealFrac Float

instance RealFloat Float

data Double

instance Eq Double

instance Ord Double

instance Num Double

instance Real Double

instance Fractional Double

instance Floating Double

instance RealFrac Double

instance RealFloat Double

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

data Word

numericEnumFrom :: (Fractional a) => a -> [a]
numericEnumFromThen :: (Fractional a) => a -> a -> [a]
numericEnumFromTo :: (Fractional a, Ord a) => a -> a -> [a]
numericEnumFromThenTo :: (Fractional a, Ord a) => a -> a -> a -> [a]
numericEnumFrom = iterate (+ 1)

numericEnumFromThen n m = iterate (+ (m - n)) n

numericEnumFromTo n m = takeWhile (<= m + 1 / 2) (numericEnumFrom n)

numericEnumFromThenTo n n' m = takeWhile p (numericEnumFromThen n n')
  where
    p
      | n' >= n = (<= m + (n' - n) / 2)
      | otherwise = (>= m + (n' - n) / 2)

instance (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => Bounded (a, b, c, d, e) where
  minBound = (minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) => Bounded (a, b, c, d, e, f) where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

instance
  (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g) =>
  Bounded (a, b, c, d, e, f, g)
  where
  minBound = (minBound, minBound, minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)

fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x = x
  | otherwise = until p f (f x)

asTypeOf :: a -> a -> a
asTypeOf = const

error :: String -> a
error = errorText . pack

errorWithoutStackTrace :: [Char] -> a
errorWithoutStackTrace = placeholder

undefined :: a
undefined = error "Prelude.undefined"

placeholder :: a
placeholder = error "Prelude.placeholder"

trace :: String -> a -> a
trace = traceText . pack
