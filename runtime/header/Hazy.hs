{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- The compiler primitives are a combination of declarations that are hard wired
-- into the compiler from Hazy.Builtin and primitives that are defined in
-- userspace but left abstract.

-- |
-- This is the main module for accessing Hazy's internals. This module exports
-- all the compiler primitives and Hazy's internal Prelude.
module Hazy (module Hazy, module Hazy.Builtin, module Hazy.Helper, module Hazy.Prelude) where

import Hazy.Builtin
import Hazy.Helper
import Hazy.Prelude

-- The usage of 'missing' variable is deliberate. It's not a special compiler
-- builtin, but rather the compiler doesn't bother doing symbol resolution on
-- definitions it doesn't need.

error :: Text -> a
error = missing

pack :: [Char] -> Text
pack = missing

putStrLn :: Text -> IO ()
putStrLn = missing

trace :: Text -> a -> a
trace = missing

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
