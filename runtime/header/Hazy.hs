{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- |
-- This is the module for accessing primitives defined in Javascript
module Hazy where

import {-# BUILTIN #-} Hazy.Builtin
import Hazy.Prelude (GeneralCategory, IO, String, Text)

errorText :: !(Text -> a)
pack :: !(String -> Text)
unpack :: !(Text -> String)
putStrLnText :: !(Text -> IO ())
traceText :: !(Text -> a -> a)
generalCategory :: !(Char -> GeneralCategory)
primIntToChar :: !(Int -> Char)
primCharToInt :: !(Char -> Int)
primEqualInt, primLessThenEqualInt :: !(Int -> Int -> Bool)
primIntMinBound, primIntMaxBound :: !Int
primIntAdd, primIntMinus, primIntMultiply :: !(Int -> Int -> Int)
primIntNegate :: !(Int -> Int)
primIntAbs, primIntSignum :: !(Int -> Int)
primIntQuot, primIntRem :: !(Int -> Int -> Int)
primIntToInteger :: !(Int -> Integer)
primIntegerCastToInt, primIntegerTruncateToInt :: !(Integer -> Int)
primEqualInteger, primLessThenEqualInteger :: !(Integer -> Integer -> Bool)
primIntegerEqual, primIntegerLessThenEqual :: !(Integer -> Integer -> Bool)
primIntegerAdd, primIntegerMinus, primIntegerMultiply :: !(Integer -> Integer -> Integer)
primIntegerNegate :: !(Integer -> Integer)
primIntegerAbs, primIntegerSignum :: !(Integer -> Integer)
primIntegerQuot, primIntegerRem :: !(Integer -> Integer -> Integer)
primSTPure :: !(a -> ST s a)
primSTBind :: !(ST s a -> (a -> ST s b) -> ST s b)
