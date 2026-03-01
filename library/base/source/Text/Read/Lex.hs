module Text.Read.Lex
  ( Lexeme (..),
    Number,
    numberToInteger,
    numberToFixed,
    numberToRational,
    numberToRangedRational,
    lex,
    expect,
    hsLex,
    lexChar,
    readBinP,
    readIntP,
    readOctP,
    readDecP,
    readHexP,
    isSymbolChar,
  )
where

import Data.Bool (Bool)
import Data.Char (Char)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Ratio (Rational)
import Data.String (String)
import Hazy (placeholder)
import Text.ParserCombinators.ReadP (ReadP)
import Prelude (Integer, Num, error)

data Lexeme
  = Char Char
  | String String
  | Punc String
  | Ident String
  | Symbol String
  | Number Number
  | EOF

data Number

numberToInteger :: Number -> Maybe Integer
numberToInteger = placeholder

numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed = placeholder

numberToRational :: Number -> Rational
numberToRational = placeholder

numberToRangedRational :: (Int, Int) -> Number -> Maybe Rational
numberToRangedRational = placeholder

lex :: ReadP Lexeme
lex = placeholder

expect :: Lexeme -> ReadP ()
expect = placeholder

hsLex :: ReadP String
hsLex = placeholder

lexChar :: ReadP Char
lexChar = placeholder

readBinP :: (Eq a, Num a) => ReadP a
readBinP = placeholder

readIntP :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP = placeholder

readOctP :: (Eq a, Num a) => ReadP a
readOctP = placeholder

readDecP :: (Eq a, Num a) => ReadP a
readDecP = placeholder

readHexP :: (Eq a, Num a) => ReadP a
readHexP = placeholder

isSymbolChar :: Char -> Bool
isSymbolChar = placeholder
