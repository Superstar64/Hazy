module Numeric
  ( showSigned,
    showIntAtBase,
    showInt,
    showBin,
    showHex,
    showOct,
    showEFloat,
    showFFloat,
    showGFloat,
    showFFloatAlt,
    showGFloatAlt,
    showFloat,
    showHFloat,
    floatToDigits,
    readSigned,
    readInt,
    readBin,
    readDec,
    readOct,
    readHex,
    readFloat,
    lexDigits,
    fromRat,
    Floating,
  )
where

import Hazy.Prelude (placeholder)

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned = placeholder

showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase = placeholder

showInt :: (Integral a) => a -> ShowS
showInt = placeholder

showBin :: (Integral a) => a -> ShowS
showBin = placeholder

showHex :: (Integral a) => a -> ShowS
showHex = placeholder

showOct :: (Integral a) => a -> ShowS
showOct = placeholder

showEFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat = placeholder

showFFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat = placeholder

showGFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat = placeholder

showFFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloatAlt = placeholder

showGFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloatAlt = placeholder

showFloat :: (RealFloat a) => a -> ShowS
showFloat = placeholder

showHFloat :: (RealFloat a) => a -> ShowS
showHFloat = placeholder

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits = placeholder

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned = placeholder

readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt = placeholder

readBin :: (Eq a, Num a) => ReadS a
readBin = placeholder

readDec :: (Eq a, Num a) => ReadS a
readDec = placeholder

readOct :: (Eq a, Num a) => ReadS a
readOct = placeholder

readHex :: (Eq a, Num a) => ReadS a
readHex = placeholder

readFloat :: (RealFrac a) => ReadS a
readFloat = placeholder

lexDigits :: ReadS String
lexDigits = placeholder

fromRat :: (RealFloat a) => Rational -> a
fromRat = placeholder
