{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This module contains helpers that the runtime call into. This is mainly
-- for type class defaults and instances that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy
import Hazy.Prelude

defaultEqual, defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultEqual x y = not (x /= y)
defaultNotEqual x y = not (x == y)

defaultCompare :: (Ord a) => a -> a -> Ordering
defaultCompare x y
  | x == y = EQ
  | x <= y = LT
  | otherwise = GT

defaultLessThen,
  defaultLessThenEqual,
  defaultGreaterThen,
  defaultGreaterThenEqual ::
    (Ord a) => a -> a -> Bool
defaultLessThen x y = compare x y == LT
defaultLessThenEqual x y = compare x y /= GT
defaultGreaterThen x y = compare x y == GT
defaultGreaterThenEqual x y = compare x y /= LT

defaultMax, defaultMin :: (Ord a) => a -> a -> a
defaultMax x y
  | x <= y = y
  | otherwise = x
defaultMin x y
  | x <= y = x
  | otherwise = y

defaultToRational :: (Real a) => a -> Ratio Integer
defaultToRational = undefined

defaultQuot,
  defaultRem,
  defaultDiv,
  defaultMod ::
    (Integral a) => a -> a -> a
defaultQuot n d = q where (q, r) = quotRem n d
defaultRem n d = r where (q, r) = quotRem n d
defaultDiv n d = q where (q, r) = divMod n d
defaultMod n d = r where (q, r) = divMod n d

defaultQuotRem, defaultDivMod :: (Integral a) => a -> a -> (a, a)
defaultQuotRem = undefined
defaultDivMod n d = if signum r == negate (signum d) then (q - 1, r + d) else qr
  where
    qr@(q, r) = quotRem n d

defaultToInteger :: (Integral a) => a -> Integer
defaultToInteger = undefined

defaultDivide :: (Fractional a) => a -> a -> a
defaultDivide x y = x * recip y

defaultRecip :: (Fractional a) => a -> a
defaultRecip x = 1 / x

defaultFromRational :: (Fractional a) => Ratio Integer -> a
defaultFromRational = undefined

defaultSucc, defaultPred :: (Enum a) => a -> a
defaultSucc = toEnum . (\x -> x + 1) . fromEnum
defaultPred = toEnum . (subtract 1) . fromEnum

defaultToEnum :: (Enum a) => Int -> a
defaultToEnum = undefined

defaultFromEnum :: (Enum a) => a -> Int
defaultFromEnum = undefined

defaultEnumFrom :: (Enum a) => a -> [a]
defaultEnumFrom x = map toEnum [fromEnum x ..]

defaultEnumFromTo :: (Enum a) => a -> a -> [a]
defaultEnumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

defaultEnumFromThen :: (Enum a) => a -> a -> [a]
defaultEnumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

defaultEnumFromThenTo :: (Enum a) => a -> a -> a -> [a]
defaultEnumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

defaultPlus, defaultMinus, defaultMultiply :: (Num a) => a -> a -> a
defaultPlus = undefined
defaultMinus x y = x + negate y
defaultMultiply = undefined

defaultNegate :: (Num a) => a -> a
defaultNegate x = 0 - x

defaultAbs, defaultSignum :: (Num a) => a -> a
defaultAbs = undefined
defaultSignum = undefined

defaultFromInteger :: (Num a) => Integer -> a
defaultFromInteger = undefined

defaultFmap :: (Functor f) => (a -> b) -> f a -> f b
defaultFmap = undefined

defaultFconst :: (Functor f) => a -> f b -> f a
defaultFconst a = fmap (const a)

defaultPure :: (Applicative f) => a -> f a
defaultPure = undefined

defaultAp :: (Applicative f) => f (a -> b) -> f a -> f b
defaultAp = liftA2 id

defaultLiftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
defaultLiftA2 f a b = f <$> a <*> b

defaultDiscardLeft :: (Applicative f) => f a -> f b -> f b
defaultDiscardLeft = liftA2 (flip const)

defaultDiscardRight :: (Applicative f) => f a -> f b -> f a
defaultDiscardRight = liftA2 const

defaultBind :: (Monad m) => m a -> (a -> m b) -> m b
defaultBind = undefined

defaultThen :: (Monad m) => m a -> m b -> m b
defaultThen a b = a >>= \_ -> b

defaultReturn :: (Monad m) => a -> m a
defaultReturn = pure

defaultFail :: (MonadFail m) => String -> m a
defaultFail = undefined

newtype HelperBool = Bool Bool

instance Eq HelperBool where
  Bool False == Bool False = True
  Bool True == Bool True = True
  _ == _ = False

instance Ord HelperBool where
  Bool False <= _ = True
  Bool True <= Bool True = True
  _ <= _ = False

instance Enum HelperBool where
  toEnum = \case
    0 -> Bool False
    1 -> Bool True
  fromEnum = \case
    Bool False -> 0
    Bool True -> 1

newtype HelperChar = Char Char

instance Eq HelperChar where
  Char c == Char c' = fromEnum c == fromEnum c'

instance Ord HelperChar where
  Char c <= Char c' = fromEnum c <= fromEnum c'

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

newtype HelperOrdering = Ordering Ordering

instance Eq HelperOrdering where
  Ordering LT == Ordering LT = True
  Ordering EQ == Ordering EQ = True
  Ordering GT == Ordering GT = True
  _ == _ = False

newtype HelperList a = List {list :: [a]}

instance Functor HelperList where
  fmap f (List xs) = List (map f xs)

instance Applicative HelperList where
  pure x = List [x]
  (<*>) = ap

instance Monad HelperList where
  List m >>= k = List $ concat $ map (list . k) m

instance MonadFail HelperList where
  fail _ = List []
