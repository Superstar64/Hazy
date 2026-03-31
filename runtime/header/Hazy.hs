{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This is the module for accessing primitives defined in Javascript
module Hazy where

import Hazy.Builtin
import Hazy.Prelude (GeneralCategory, IO, Text)

-- The usage of 'missing' variable is deliberate. It's not a special compiler
-- builtin, but rather the compiler doesn't bother doing symbol resolution on
-- definitions it doesn't need.

errorText :: Text -> a
errorText = missing

pack :: [Char] -> Text
pack = missing

putStrLnText :: Text -> IO ()
putStrLnText = missing

traceText :: Text -> a -> a
traceText = missing

generalCategory :: Char -> GeneralCategory
generalCategory = missing

primToConstructorTag :: a -> Int
primToConstructorTag = missing

-- the constructor must be unitary
primFromConstructorTag :: Int -> a
primFromConstructorTag = missing

primIntToChar :: Int -> Char
primIntToChar = missing

primCharToInt :: Char -> Int
primCharToInt = missing

primEqualInt, primLessThenEqualInt :: Int -> Int -> Bool
primEqualInt = missing
primLessThenEqualInt = missing

primIntMinBound, primIntMaxBound :: Int
primIntMinBound = missing
primIntMaxBound = missing

primIntAdd, primIntMinus, primIntMultiply :: Int -> Int -> Int
primIntAdd = missing
primIntMinus = missing
primIntMultiply = missing

primIntNegate :: Int -> Int
primIntNegate = missing

primIntAbs, primIntSignum :: Int -> Int
primIntAbs = missing
primIntSignum = missing

primEqualInteger, primLessThenEqualInteger :: Integer -> Integer -> Bool
primEqualInteger = missing
primLessThenEqualInteger = missing

primIntToInteger :: Int -> Integer
primIntToInteger = missing

primIntQuot, primIntRem :: Int -> Int -> Int
primIntQuot = missing
primIntRem = missing

primIntegerCastToInt, primIntegerTruncateToInt :: Integer -> Int
primIntegerCastToInt = missing
primIntegerTruncateToInt = missing

primIntegerEqual, primIntegerLessThenEqual :: Integer -> Integer -> Bool
primIntegerEqual = missing
primIntegerLessThenEqual = missing

primIntegerAdd, primIntegerMinus, primIntegerMultiply :: Integer -> Integer -> Integer
primIntegerAdd = missing
primIntegerMinus = missing
primIntegerMultiply = missing

primIntegerNegate :: Integer -> Integer
primIntegerNegate = missing

primIntegerAbs, primIntegerSignum :: Integer -> Integer
primIntegerAbs = missing
primIntegerSignum = missing

primIntegerQuot, primIntegerRem :: Integer -> Integer -> Integer
primIntegerQuot = missing
primIntegerRem = missing

primSTPure :: a -> ST s a
primSTPure = missing

primSTBind :: ST s a -> (a -> ST s b) -> ST s b
primSTBind = missing
