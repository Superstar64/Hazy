module Hazy.PreludeNumeric where

import Hazy.Prelude

showSigned :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showSigned = placeholder

showInt :: (Integral a) => a -> ShowS
showInt = placeholder

readSigned :: (Real a) => ReadS a -> ReadS a
readSigned = placeholder

readDec :: (Eq a, Num a) => ReadS a
readDec = placeholder

showFloat :: (RealFloat a) => a -> ShowS
showFloat = placeholder

readFloat :: (RealFrac a) => ReadS a
readFloat = placeholder

lexDigits :: ReadS String
lexDigits = placeholder
