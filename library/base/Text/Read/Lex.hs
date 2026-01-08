{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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
numberToInteger = error "todo"

numberToFixed :: Integer -> Number -> Maybe (Integer, Integer)
numberToFixed = error "todo"

numberToRational :: Number -> Rational
numberToRational = error "todo"

numberToRangedRational :: (Int, Int) -> Number -> Maybe Rational
numberToRangedRational = error "todo"

lex :: ReadP Lexeme
lex = error "todo"

expect :: Lexeme -> ReadP ()
expect = error "todo"

hsLex :: ReadP String
hsLex = error "todo"

lexChar :: ReadP Char
lexChar = error "todo"

readBinP :: (Eq a, Num a) => ReadP a
readBinP = error "todo"

readIntP :: (Num a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
readIntP = error "todo"

readOctP :: (Eq a, Num a) => ReadP a
readOctP = error "todo"

readDecP :: (Eq a, Num a) => ReadP a
readDecP = error "todo"

readHexP :: (Eq a, Num a) => ReadP a
readHexP = error "todo"

isSymbolChar :: Char -> Bool
isSymbolChar = error "todo"
