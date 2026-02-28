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

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned = error "todo"

showIntAtBase :: (Integral a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase = error "todo"

showInt :: (Integral a) => a -> ShowS
showInt = error "todo"

showBin :: (Integral a) => a -> ShowS
showBin = error "todo"

showHex :: (Integral a) => a -> ShowS
showHex = error "todo"

showOct :: (Integral a) => a -> ShowS
showOct = error "todo"

showEFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showEFloat = error "todo"

showFFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat = error "todo"

showGFloat :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat = error "todo"

showFFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloatAlt = error "todo"

showGFloatAlt :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloatAlt = error "todo"

showFloat :: (RealFloat a) => a -> ShowS
showFloat = error "todo"

showHFloat :: (RealFloat a) => a -> ShowS
showHFloat = error "todo"

floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
floatToDigits = error "todo"

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned = error "todo"

readInt :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt = error "todo"

readBin :: (Eq a, Num a) => ReadS a
readBin = error "todo"

readDec :: (Eq a, Num a) => ReadS a
readDec = error "todo"

readOct :: (Eq a, Num a) => ReadS a
readOct = error "todo"

readHex :: (Eq a, Num a) => ReadS a
readHex = error "todo"

readFloat :: (RealFrac a) => ReadS a
readFloat = error "todo"

lexDigits :: ReadS String
lexDigits = error "todo"

fromRat :: (RealFloat a) => Rational -> a
fromRat = error "todo"
