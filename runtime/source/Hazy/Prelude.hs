{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This module contains Hazy's Prelude variant. As of now, this is largely
-- subset of the proper Prelude that is needed by helper code.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Prelude
  ( module Hazy.Prelude,
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

infixr 9 .

infixr 8 ^, ^^, **

infixl 4 <$>

infixr 3 &&

infixr 2 ||

infixr 1 =<<

infixr 0 $, $!, `seq`

infixl 9 !!

infixr 5 ++

infix 4 `elem`, `notElem`

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
        negate (atan2 (-y) x)
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
x ^^ n = if n >= 0 then x ^ n else recip (x ^ (negate n))

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

($) :: (a -> b) -> a -> b
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

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: [a] -> Int
length [] = 0
length (_ : l) = 1 + length l

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

-- todo replace with Foldable

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =
  q
    : ( case xs of
          [] -> []
          x : xs -> scanl f (f q x) xs
      )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x : xs')
  | p x = (x : ys, zs)
  | otherwise = ([], xs)
  where
    (ys, zs) = span p xs'
break p = span (not . p)

lines :: String -> [String]
lines "" = []
lines s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines s''

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s''
    where
      (w, s'') = break isSpace s'

isSpace :: Char -> Bool
isSpace = placeholder

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ' : s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (a -> Bool) -> [a] -> Bool
any p = or . map p
all p = and . map p

elem, notElem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)
notElem x = all (/= x)

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x, y) : xys)
  | key == x = Just y
  | otherwise = lookup key xys

sum, product :: (Num a) => [a] -> a
sum = foldl (+) 0
product = foldl (*) 1

maximum, minimum :: (Ord a) => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a : as) (b : bs) =
  z a b : zipWith z as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a : as) (b : bs) (c : cs) =
  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr
    (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs))
    ([], [], [])

placeholder :: a
placeholder = error "Prelude.placeholder"

trace :: String -> a -> a
trace = traceText . pack

putStrLn :: String -> IO ()
putStrLn = putStrLnText . pack

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
