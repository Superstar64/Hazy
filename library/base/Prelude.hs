{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Prelude
  ( Bool (..),
    (&&),
    (||),
    not,
    otherwise,
    Maybe (..),
    maybe,
    Either (..),
    either,
    Ordering (..),
    Char,
    String,
    fst,
    snd,
    curry,
    uncurry,
    Eq (..),
    Ord (..),
    Enum (..),
    Bounded (..),
    Int,
    Integer,
    Float,
    Double,
    Rational,
    Word,
    Num (..),
    Real (..),
    Integral (..),
    Fractional (..),
    Floating (..),
    RealFrac (..),
    RealFloat (..),
    subtract,
    even,
    odd,
    gcd,
    lcm,
    (^),
    (^^),
    fromIntegral,
    realToFrac,
    Semigroup (..),
    Monoid (..),
    Functor (..),
    (<$>),
    Applicative (..),
    Monad (..),
    MonadFail (..),
    mapM_,
    sequence_,
    (=<<),
    Foldable (..),
    Traversable (..),
    id,
    const,
    (.),
    flip,
    ($),
    until,
    asTypeOf,
    error,
    errorWithoutStackTrace,
    undefined,
    seq,
    ($!),
    map,
    (++),
    filter,
    head,
    last,
    tail,
    init,
    (!!),
    null,
    length,
    reverse,
    and,
    or,
    any,
    all,
    concat,
    concatMap,
    scanl,
    scanl1,
    scanr,
    scanr1,
    iterate,
    repeat,
    replicate,
    cycle,
    take,
    drop,
    takeWhile,
    dropWhile,
    span,
    break,
    splitAt,
    notElem,
    lookup,
    zip,
    zip3,
    zipWith,
    zipWith3,
    unzip,
    unzip3,
    lines,
    words,
    unlines,
    unwords,
    ShowS,
    Show (..),
    shows,
    showChar,
    showString,
    showParen,
    ReadS,
    Read (..),
    reads,
    readParen,
    read,
    lex,
    IO,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    interact,
    FilePath,
    readFile,
    writeFile,
    appendFile,
    readIO,
    readLn,
    IOError,
    ioError,
    userError,
  )
where

import Control.Applicative
  ( Applicative
      ( pure,
        (*>),
        (<*),
        (<*>)
      ),
  )
import Control.Monad
  ( Monad
      ( return,
        (>>),
        (>>=)
      ),
    MonadFail
      ( fail
      ),
    mapM_,
    sequence_,
    (=<<),
  )
import Data.Bool
  ( Bool (False, True),
    not,
    otherwise,
    (&&),
    (||),
  )
import Data.Char (Char)
import Data.Either
  ( Either (Left, Right),
    either,
  )
import Data.Eq
  ( Eq
      ( (/=),
        (==)
      ),
  )
import Data.Foldable
  ( Foldable
      ( elem,
        fold,
        foldMap,
        foldl,
        foldl1,
        foldr,
        foldr1,
        length,
        maximum,
        minimum,
        null,
        product,
        sum,
        toList
      ),
  )
import Data.Function
  ( const,
    flip,
    id,
    ($),
    (.),
  )
import Data.Functor (Functor (fmap, (<$)), (<$>))
import Data.Int (Int)
import Data.List
  ( all,
    and,
    any,
    break,
    concat,
    concatMap,
    cycle,
    drop,
    dropWhile,
    filter,
    head,
    init,
    iterate,
    last,
    length,
    lookup,
    map,
    notElem,
    null,
    or,
    repeat,
    replicate,
    reverse,
    scanl,
    scanl1,
    scanr,
    scanr1,
    span,
    splitAt,
    tail,
    take,
    takeWhile,
    unzip,
    unzip3,
    zip,
    zip3,
    zipWith,
    zipWith3,
    (!!),
    (++),
  )
import Data.Maybe
  ( Maybe (Just, Nothing),
    maybe,
  )
import Data.Monoid (Monoid (mappend, mconcat, mempty))
import Data.Ord
  ( Ord
      ( compare,
        max,
        min,
        (<),
        (<=),
        (>),
        (>=)
      ),
    Ordering (EQ, GT, LT),
  )
import Data.Ratio (Rational)
import Data.Semigroup (Semigroup ((<>)))
import Data.String
  ( String,
    lines,
    unlines,
    unwords,
    words,
  )
import Data.Traversable
  ( Traversable
      ( mapM,
        sequence,
        sequenceA,
        traverse
      ),
  )
import Data.Tuple
  ( curry,
    fst,
    snd,
    uncurry,
  )
import Hazy
  ( Enum (..),
    Integer,
    Num (..),
    placeholder,
  )
import System.IO
  ( FilePath,
    IO,
    appendFile,
    getChar,
    getContents,
    getLine,
    interact,
    print,
    putChar,
    putStr,
    putStrLn,
    readFile,
    readIO,
    readLn,
    writeFile,
  )
import Text.Read
  ( Read
      ( readList,
        readsPrec
      ),
    ReadS,
    lex,
    read,
    readParen,
    reads,
  )
import Text.Show
  ( Show
      ( show,
        showList,
        showsPrec
      ),
    ShowS,
    showChar,
    showParen,
    showString,
    shows,
  )
import Prelude ()

class Bounded a where
  minBound, maxBound :: a

data Float

data Double

data Word

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
  infixl 7 `quot`, `rem`, `div`, `mod`
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer

class (Num a) => Fractional a where
  infixl 7 /
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

class (Fractional a) => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  infixr 8 **
  (**), logBase :: a -> a -> a
  sin, cos, tan :: a -> a
  asin, acos, atan :: a -> a
  sinh, cosh, tanh :: a -> a
  asinh, acosh, atanh :: a -> a

class (Real a, Fractional a) => RealFrac a where
  properFraction :: (Integral b) => a -> (b, a)
  truncate :: (Integral b) => a -> b
  round :: (Integral b) => a -> b
  ceiling :: (Integral b) => a -> b
  floor :: (Integral b) => a -> b

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a

subtract :: (Num a) => a -> a -> a
subtract = error "todo"

even :: (Integral a) => a -> Bool
even = error "todo"

odd :: (Integral a) => a -> Bool
odd = error "todo"

gcd :: (Integral a) => a -> a -> a
gcd = error "todo"

lcm :: (Integral a) => a -> a -> a
lcm = error "todo"

infixr 8 ^

(^) :: (Num a, Integral b) => a -> b -> a
(^) = error "todo"

infixr 8 ^^

(^^) :: (Fractional a, Integral b) => a -> b -> a
(^^) = error "todo"

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = error "todo"

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = error "todo"

until :: (a -> Bool) -> (a -> a) -> a -> a
until = error "todo"

asTypeOf :: a -> a -> a
asTypeOf = error "todo"

error :: [Char] -> a
error _ = placeholder

errorWithoutStackTrace :: [Char] -> a
errorWithoutStackTrace = error "todo"

undefined :: a
undefined = placeholder

infixr 0 `seq`

seq :: a -> b -> b
seq = error "todo"

infixr 0 $!

($!) :: (a -> b) -> a -> b
($!) = error "todo"

data IOError

ioError :: IOError -> IO a
ioError = error "todo"

userError :: String -> IOError
userError = error "todo"
