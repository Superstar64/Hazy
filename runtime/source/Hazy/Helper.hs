-- |
-- This module contains helpers that the runtime call into. This is mainly
-- for type class defaults and instances that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy
import Hazy.Prelude

data Strict a = Strict !a

enumEqual :: (Enum a) => a -> a -> Bool
enumEqual l r = fromEnum l == fromEnum r

enumCompare :: (Enum a) => a -> a -> Ordering
enumCompare l r = compare (fromEnum l) (fromEnum r)

ratPrec = 7 :: Int

reduce :: (Integral a) => a -> a -> HelperRatio a
reduce _ 0 = error "Data.Ratio.% : zero denominator"
reduce x y = Ratio $ (x `quot` d) :% (y `quot` d)
  where
    d = gcd x y

notEqual :: (Eq a) => a -> a -> Bool
notEqual x y = not (x == y)

lessThen,
  lessThenEqual,
  greaterThen,
  greaterThenEqual ::
    (Ord a) => a -> a -> Bool
lessThen x y = compare x y == LT
lessThenEqual x y = compare x y /= GT
greaterThen x y = compare x y == GT
greaterThenEqual x y = compare x y /= LT

larger, smaller :: (Ord a) => a -> a -> a
larger x y
  | x <= y = y
  | otherwise = x
smaller x y
  | x <= y = x
  | otherwise = y

newtype HelperBool = Bool Bool

instance Eq HelperBool where
  (==) = enumEqual

instance Ord HelperBool where
  compare = enumCompare

instance Enum HelperBool where
  toEnum x | x >= 0 && x < 2 = primFromConstructorTag x
  fromEnum = primToConstructorTag

newtype HelperChar = Char Char

instance Eq HelperChar where
  (==) = enumEqual

instance Ord HelperChar where
  compare = enumCompare

instance Enum HelperChar where
  toEnum x = Char (primIntToChar x)
  fromEnum (Char x) = primCharToInt x

newtype HelperInt = Int Int

instance Eq HelperInt where
  Int x == Int y = primEqualInt x y

instance Ord HelperInt where
  Int x <= Int y = primLessThenEqualInt x y

instance Enum HelperInt where
  succ (Int x) = Int (x + 1)
  pred (Int x) = Int (x - 1)
  toEnum = Int
  fromEnum (Int x) = x
  enumFrom (Int x) = [Int x .. Int maxBound]
  enumFromTo (Int x) (Int y) = [Int x, Int (x + 1) .. Int y]
  enumFromThen (Int x) (Int y) = [Int x, Int y .. Int maxBound]
  enumFromThenTo (Int from) (Int thenx) (Int to) = run from
    where
      run from | from > to = []
      run from = Int from : run (from + step)
      step = thenx - from

instance Num HelperInt where
  Int x + Int y = Int (primIntAdd x y)
  Int x - Int y = Int (primIntMinus x y)
  Int x * Int y = Int (primIntMultiply x y)
  negate (Int x) = Int (primIntNegate x)
  abs (Int x) = Int (primIntAbs x)
  signum (Int x) = Int (primIntSignum x)
  fromInteger x = Int (primIntegerTruncateToInt x)

instance Real HelperInt where
  toRational (Int x) = primIntToInteger x :% 1

instance Integral HelperInt where
  Int x `quot` Int y = Int (primIntQuot x y)
  Int x `rem` Int y = Int (primIntRem x y)
  quotRem x y = (x `quot` y, x `rem` y)
  toInteger (Int x) = primIntToInteger x

newtype HelperInteger = Integer Integer

instance Eq HelperInteger where
  Integer x == Integer y = primEqualInteger x y

instance Ord HelperInteger where
  Integer x <= Integer y = primLessThenEqualInteger x y

instance Enum HelperInteger where
  succ (Integer x) = Integer (x + 1)
  pred (Integer x) = Integer (x - 1)
  toEnum x = Integer (primIntToInteger x)
  fromEnum (Integer x) = primIntegerCastToInt x
  enumFrom (Integer x) = [Integer x, Integer (x + 1) ..]
  enumFromTo (Integer x) (Integer y) = [Integer x, Integer (x + 1) .. Integer y]
  enumFromThen (Integer from) (Integer thenx) = run from
    where
      run from = Integer from : run (from + step)
      step = thenx - from
  enumFromThenTo (Integer from) (Integer thenx) (Integer to) = run from
    where
      run from | from > to = []
      run from = Integer from : run (from + step)
      step = thenx - from

instance Num HelperInteger where
  Integer x + Integer y = Integer (primIntegerAdd x y)
  Integer x - Integer y = Integer (primIntegerMinus x y)
  Integer x * Integer y = Integer (primIntegerMultiply x y)
  negate (Integer x) = Integer (primIntegerNegate x)
  abs (Integer x) = Integer (primIntegerAbs x)
  signum (Integer x) = Integer (primIntegerSignum x)
  fromInteger x = Integer x

instance Real HelperInteger where
  toRational (Integer x) = x :% 1

instance Integral HelperInteger where
  Integer x `quot` Integer y = Integer (primIntegerQuot x y)
  Integer x `rem` Integer y = Integer (primIntegerRem x y)
  quotRem x y = (x `quot` y, x `rem` y)
  toInteger (Integer x) = x

newtype HelperOrdering = Ordering Ordering

instance Eq HelperOrdering where
  (==) = enumEqual

instance Ord HelperOrdering where
  compare = enumCompare

instance Enum HelperOrdering where
  toEnum x | x >= 0 && x < 3 = primFromConstructorTag x
  fromEnum = primToConstructorTag

newtype HelperList a = List {list :: [a]}

instance (Eq a) => Eq (HelperList a) where
  List [] == List [] = True
  List (x : xs) == List (x' : xs') = x == x' && xs == xs'
  _ == _ = False

instance (Ord a) => Ord (HelperList a) where
  List [] `compare` List [] = EQ
  List [] `compare` List (_ : _) = LT
  List (_ : _) `compare` List [] = GT
  List (x : xs) `compare` List (x' : xs') = case compare x x' of
    LT -> LT
    EQ -> compare xs xs'
    GT -> GT

instance Functor HelperList where
  fmap f (List xs) = List (map f xs)

instance Applicative HelperList where
  pure x = List [x]
  (<*>) = ap

instance Monad HelperList where
  List m >>= k = List $ concat $ map (list . k) m

instance MonadFail HelperList where
  fail _ = List []

instance Semigroup (HelperList a) where
  List xs <> List ys = List (xs ++ ys)

instance Monoid (HelperList a) where
  mempty = List []

newtype HelperNonEmpty a = NonEmpty {nonEmpty :: NonEmpty a}

instance (Eq a) => Eq (HelperNonEmpty a) where
  NonEmpty (x :| xs) == NonEmpty (x' :| xs') = x == x' && xs == xs'

instance (Ord a) => Ord (HelperNonEmpty a) where
  NonEmpty (x :| xs) `compare` NonEmpty (x' :| xs') = case compare x x' of
    LT -> LT
    EQ -> compare xs xs'
    GT -> GT

instance Functor HelperNonEmpty where
  fmap f (NonEmpty (x :| xs)) = NonEmpty (f x :| fmap f xs)

instance Applicative HelperNonEmpty where
  pure x = NonEmpty (x :| [])
  (<*>) = ap

instance Monad HelperNonEmpty where
  NonEmpty (x :| xs) >>= k = NonEmpty $ case k x of
    NonEmpty (y :| ys) -> y :| ys ++ (xs >>= toList . k)
      where
        toList (NonEmpty (x :| xs)) = x : xs

instance Semigroup (HelperNonEmpty a) where
  NonEmpty (x :| xs) <> NonEmpty (y :| ys) = NonEmpty (x :| xs ++ y : ys)

newtype HelperST s a = STx {st :: ST s a}

instance Functor (HelperST s) where
  fmap = liftM

instance Applicative (HelperST s) where
  pure x = STx (primSTPure x)
  (<*>) = ap

instance Monad (HelperST s) where
  STx m >>= f = STx (primSTBind m (st . f))

newtype HelperUnit = HelperUnit ()

instance Enum HelperUnit where
  fromEnum (HelperUnit ()) = 0
  toEnum 0 = HelperUnit ()

newtype HelperRatio a = Ratio {ratio :: Ratio a}

instance (Eq a) => Eq (HelperRatio a) where
  Ratio (n :% d) == Ratio (n' :% d') =
    n == n' && d == d'

instance (Integral a) => Ord (HelperRatio a) where
  Ratio (x :% y) <= Ratio (x' :% y') = x * y' <= x' * y
  Ratio (x :% y) < Ratio (x' :% y') = x * y' < x' * y

instance (Integral a) => Num (HelperRatio a) where
  Ratio (x :% y) + Ratio (x' :% y') = reduce (x * y' + x' * y) (y * y')
  Ratio (x :% y) * Ratio (x' :% y') = reduce (x * x') (y * y')
  negate (Ratio (x :% y)) = Ratio ((negate x) :% y)
  abs (Ratio (x :% y)) = Ratio (abs x :% y)
  signum (Ratio (x :% y)) = Ratio (signum x :% 1)
  fromInteger x = Ratio (fromInteger x :% 1)

instance (Integral a) => Real (HelperRatio a) where
  toRational (Ratio (x :% y)) = toInteger x :% toInteger y

instance (Integral a) => Fractional (HelperRatio a) where
  Ratio (x :% y) / Ratio (x' :% y') = Ratio $ (x * y') % (y * x')
  recip (Ratio (x :% y)) = Ratio (y % x)
  fromRational (x :% y) = Ratio (fromInteger x :% fromInteger y)

instance (Integral a) => Enum (HelperRatio a) where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate . ratio
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo
