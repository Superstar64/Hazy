-- |
-- This module contains helpers that the runtime call into. This is mainly
-- for type class defaults and instances that are necessarily part of the
-- runtime by transative closure.
--
-- The definitions here are largely taken from the Haskell2010 report.
module Hazy.Helper where

import Hazy
import Hazy.Prelude

minilex :: ReadS Char
minilex (c : s) = case c of
              ' ' -> minilex s
              '\t' -> minilex s
              '\n' -> minilex s
              '\r' -> minilex s
              '\f' -> minilex s
              '\v' -> minilex s
              _ -> [(c, s)]
minilex "" = []

minilexSingle :: Char -> ReadS ()
minilexSingle c' s = [ ((), t) | (c, t) <- minilex s, c == c' ]

bindRead :: [(a, String)] -> (a -> String -> [(b, String)]) -> [(b, String)]
bindRead as f = concatMap (\(a, s) -> f a s) as

data Strict a = Strict !a

enumEqual :: (Enum a) => a -> a -> Bool
enumEqual l r = fromEnum l == fromEnum r

enumCompare :: (Enum a) => a -> a -> Ordering
enumCompare l r = compare (fromEnum l) (fromEnum r)

reduce :: (Integral a) => a -> a -> HelperRatio a
reduce _ 0 = error "Data.Ratio.% : zero denominator"
reduce x y = Ratio $ (x `quot` d) :% (y `quot` d)
  where
    d = gcd x y

defaultNotEqual :: (Eq a) => a -> a -> Bool
defaultNotEqual x y = not (x == y)

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

defaultSconcat :: (Semigroup a) => NonEmpty a -> a
defaultSconcat = \case
  x :| [] -> x
  x :| x' : xs -> x <> sconcat (x' :| xs)

defaultStimes :: (Semigroup a, Integral b) => b -> a -> a
defaultStimes n a | n >= 1 = go n a
  where
    go 1 a = a
    go n a = a <> go (n - 1) a

defaultMappend :: (Monoid a) => a -> a -> a
defaultMappend = (<>)

defaultMconcat :: (Monoid a) => [a] -> a
defaultMconcat = \case
  [] -> mempty
  (x : xs) -> x <> mconcat xs

defaultShow :: (Show a) => a -> String
defaultShow x = showsPrec 0 x ""

defaultShowList :: (Show a) => [a] -> ShowS
defaultShowList [] = showString "[]"
defaultShowList (x : xs) = showChar '[' . shows x . showl xs
  where
    showl [] = showChar ']'
    showl (x : xs) =
      showChar ','
        . shows x
        . showl xs

defaultReadList :: Read a => ReadS [a]
defaultReadList = run
          where
            run r =
              do
                (l, s) <- minilex r
                case l of
                  '[' -> case minilex s of
                    [(']', t)] -> [([], t)]
                    _ -> readElement s
                  '(' -> do
                    (x, t) <- run s
                    (')', u) <- minilex t
                    [(x, u)]
                  _ -> []
            readElement t = do
              (x, u) <- readsPrec 0 t
              (xs, v) <- readTail u
              [(x : xs, v)]
            readTail s = do
              (l, t) <- minilex s
              case l of
                ']' -> [([], t)]
                ',' -> readElement t
                _ -> []

newtype HelperArrow a b = Arrow (a -> b)

instance (Semigroup b) => Semigroup (HelperArrow a b) where
  Arrow a <> Arrow b = Arrow (\x -> a x <> b x)

instance (Monoid b) => Monoid (HelperArrow a b) where
  mempty = Arrow (\_ -> mempty)

newtype HelperBool = Bool Bool

instance Eq HelperBool where
  (==) = enumEqual

instance Ord HelperBool where
  compare = enumCompare

instance Enum HelperBool where
  toEnum x | x >= 0 && x < 2 = primFromConstructorTag x
  fromEnum = primToConstructorTag

instance Bounded HelperBool where
  minBound = Bool False
  maxBound = Bool True

instance Show HelperBool where
  showsPrec _ (Bool bool) = case bool of
    False -> showString "False"
    True -> showString "True"

instance Read HelperBool where
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "False" -> [(Bool False, string)]
      "True" -> [(Bool True, string)]
      _ -> []

newtype HelperChar = Char Char

instance Eq HelperChar where
  (==) = enumEqual

instance Ord HelperChar where
  compare = enumCompare

instance Enum HelperChar where
  toEnum x = Char (primIntToChar x)
  fromEnum (Char x) = primCharToInt x

instance Bounded HelperChar where
  minBound = Char '\0'
  maxBound = Char '\1114111'

instance Show HelperChar where
  showsPrec p (Char '\'') = showString "'\\''"
  showsPrec p (Char c) = showChar '\'' . showLitChar c . showChar '\''

  showList cs = showChar '"' . showl cs
    where
      showl [] = showChar '"'
      showl (Char '"' : cs) = showString "\\\"" . showl cs
      showl (Char c : cs) = showLitChar c . showl cs

instance Read HelperChar where
  readsPrec p =
    readParen
      False
      ( \r ->
          [ (Char c, t)
          | ('\'' : s, t) <- lex r,
            (c, "\'") <- readLitChar s
          ]
      )

  readList =
    readParen
      False
      ( \r ->
          [ (l, t)
          | ('"' : s, t) <- lex r,
            (l, _) <- readl s
          ]
      )
    where
      readl ('"' : s) = [([], s)]
      readl ('\\' : '&' : s) = readl s
      readl s =
        [ (Char c : cs, u)
        | (c, t) <- readLitChar s,
          (cs, u) <- readl t
        ]

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

instance Bounded HelperInt where
  minBound = Int primIntMinBound
  maxBound = Int primIntMaxBound

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

instance Show HelperInt where
  showsPrec n (Int int) = showsPrec n (toInteger int)

instance Read HelperInt where
  readsPrec p r = [(Int (fromInteger i), t) | (i, t) <- readsPrec p r]

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

instance Show HelperInteger where
  showsPrec n (Integer integer) = showSigned showInt n integer

instance Read HelperInteger where
  readsPrec p s = [ (Integer i, t) | (i, t) <- readSigned readDec s ]

newtype HelperOrdering = Ordering Ordering

instance Eq HelperOrdering where
  (==) = enumEqual

instance Ord HelperOrdering where
  compare = enumCompare

instance Enum HelperOrdering where
  toEnum x | x >= 0 && x < 3 = primFromConstructorTag x
  fromEnum = primToConstructorTag

instance Bounded HelperOrdering where
  minBound = Ordering LT
  maxBound = Ordering GT

instance Semigroup HelperOrdering where
  Ordering EQ <> y = y
  x <> _ = x

instance Monoid HelperOrdering where
  mempty = Ordering EQ

instance Show HelperOrdering where
  showsPrec _ (Ordering order) = case order of
    LT -> showString "LT"
    EQ -> showString "EQ"
    GT -> showString "GT"

instance Read HelperOrdering where
  readsPrec _ string = do
    (token, string) <- lex string
    case token of
      "LT" -> [(Ordering LT, string)]
      "EQ" -> [(Ordering EQ, string)]
      "GT" -> [(Ordering GT, string)]
      _ -> []

newtype HelperList a = List {list :: [a]}

instance (Eq a) => Eq (HelperList a) where
  List [] == List [] = True
  List (x : xs) == List (x' : xs') = x == x' && xs == xs'
  _ == _ = False

instance (Ord a) => Ord (HelperList a) where
  List [] `compare` List [] = EQ
  List [] `compare` List (_ : _) = LT
  List (_ : _) `compare` List [] = GT
  List (x : xs) `compare` List (x' : xs') = compare x x' <> compare xs xs'

instance (Show a) => Show (HelperList a) where
  showsPrec p (List xs) = showList xs

instance Read a => Read (HelperList a) where
  readsPrec p s = [ (List l, t) | (l, t) <- readList s]

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
  NonEmpty (x :| xs) `compare` NonEmpty (x' :| xs') = compare x x' <> compare xs xs'

instance (Show a) => Show (HelperNonEmpty a) where
  showsPrec d (NonEmpty (x :| xs)) =
    showParen (d > 5) $
      showsPrec 6 x . showString " :| " . showsPrec 6 xs

instance (Read a) => Read (HelperNonEmpty a) where
  readsPrec d = readParen (d > 5) $ \s -> do
    (x, t) <- readsPrec 6 s
    (":|", u) <- lex t
    (xs, v) <- readsPrec 6 u
    [(NonEmpty (x :| xs), v)]

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

instance (Semigroup a) => Semigroup (HelperST s a) where
  STx a <> STx b = STx (liftA2 (<>) a b)

instance (Monoid a) => Monoid (HelperST s a) where
  mempty = pure mempty

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

instance (Show a) => Show (HelperRatio a) where
  showsPrec p (Ratio (x :% y)) =
    showParen (p > 7) $
      showsPrec (7 + 1) x
        . showString " % "
        . showsPrec (7 + 1) y

instance (Integral a, Read a) => Read (HelperRatio a) where
  readsPrec p =
    readParen
      (p > 7)
      ( \r ->
          [ (Ratio (x % y), u)
          | (x, s) <- readsPrec (7 + 1) r,
            ("%", t) <- lex s,
            (y, u) <- readsPrec (7 + 1) t
          ]
      )
