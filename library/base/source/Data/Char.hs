module Data.Char
  ( Char,
    isControl,
    isSpace,
    isLower,
    isUpper,
    isAlpha,
    isAlphaNum,
    isPrint,
    isDigit,
    isOctDigit,
    isHexDigit,
    isLetter,
    isMark,
    isNumber,
    isPunctuation,
    isSymbol,
    isSeparator,
    isAscii,
    isLatin1,
    isAsciiUpper,
    isAsciiLower,
    GeneralCategory (..),
    generalCategory,
    toUpper,
    toLower,
    toTitle,
    digitToInt,
    intToDigit,
    ord,
    chr,
    showLitChar,
    lexLitChar,
    readLitChar,
  )
where

import Hazy.Prelude
  ( Char,
    GeneralCategory (..),
    generalCategory,
    isAlpha,
    isAlphaNum,
    isDigit,
    isSpace,
    lexLitChar,
    placeholder,
    readLitChar,
    showLitChar,
  )

isControl :: Char -> Bool
isControl = placeholder

isLower :: Char -> Bool
isLower = placeholder

isUpper :: Char -> Bool
isUpper = placeholder

isPrint :: Char -> Bool
isPrint = placeholder

isOctDigit :: Char -> Bool
isOctDigit = placeholder

isHexDigit :: Char -> Bool
isHexDigit = placeholder

isLetter :: Char -> Bool
isLetter = placeholder

isMark :: Char -> Bool
isMark = placeholder

isNumber :: Char -> Bool
isNumber = placeholder

isPunctuation :: Char -> Bool
isPunctuation = placeholder

isSymbol :: Char -> Bool
isSymbol = placeholder

isSeparator :: Char -> Bool
isSeparator = placeholder

isAscii :: Char -> Bool
isAscii = placeholder

isLatin1 :: Char -> Bool
isLatin1 = placeholder

isAsciiUpper :: Char -> Bool
isAsciiUpper = placeholder

isAsciiLower :: Char -> Bool
isAsciiLower = placeholder

toUpper :: Char -> Char
toUpper = placeholder

toLower :: Char -> Char
toLower = placeholder

toTitle :: Char -> Char
toTitle = placeholder

digitToInt :: Char -> Int
digitToInt = placeholder

intToDigit :: Int -> Char
intToDigit = placeholder

ord :: Char -> Int
ord = placeholder

chr :: Int -> Char
chr = placeholder
