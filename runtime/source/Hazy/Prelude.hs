-- |
-- This module contains Hazy's Prelude variant. This is a superset of Haskell's
-- proper Prelude. The definitions here are largely taken from the Haskell2010
-- report.
module Hazy.Prelude
  ( module Hazy.Prelude,
    module Hazy.PreludeList,
    module Hazy.PreludeText,
    module Hazy.PreludeIO,
    module Hazy.Builtin,
    errorText,
    pack,
    putStrLnText,
    traceText,
    generalCategory,
  )
where

import Hazy
import Hazy.Builtin
import Hazy.Helper (Strict (..), enumCompare, enumEqual, ratio, reduce)
import Hazy.PreludeIO
import Hazy.PreludeList
import Hazy.PreludeText

infixr 9 .

infixr 8 ^, ^^, **

infixl 4 <$>

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

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

-- todo replace with Traversable

sequence :: (Monad m) => [m a] -> m [a]
sequence = foldr mcons (return [])
  where
    mcons p q = p >>= \x -> q >>= \y -> return (x : y)

sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

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

instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance Bounded Int where
  minBound = primIntMinBound
  maxBound = primIntMaxBound

-- todo `Double` and `Float`

-- todo `Bounded` for tuples

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

undefined :: a
undefined = error "Prelude.undefined"

placeholder :: a
placeholder = error "Prelude.placeholder"

trace :: String -> a -> a
trace = traceText . pack

data Text

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f a = do
  a' <- a
  return (f a')

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

type Rational = Ratio Integer

instance (Integral a) => RealFrac (Ratio a) where
  properFraction (x :% y) = (fromIntegral q, r :% y)
    where
      (q, r) = quotRem x y

(%) :: (Integral a) => a -> a -> Ratio a
numerator, denominator :: (Integral a) => Ratio a -> a
x % y = ratio (reduce (x * signum y) (abs y))

numerator (x :% _) = x

denominator (_ :% y) = y

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned

instance Enum GeneralCategory where
  toEnum x | x >= 0 && x < 30 = primFromConstructorTag x
  fromEnum = primToConstructorTag

instance Eq GeneralCategory where
  (==) = enumEqual

instance Ord GeneralCategory where
  compare = enumCompare

instance Bounded GeneralCategory where
  minBound = UppercaseLetter
  maxBound = NotAssigned
