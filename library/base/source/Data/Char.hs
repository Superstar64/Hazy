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

import Data.Bool (Bool)
import Data.Int (Int)
import Data.String (String)
import Hazy.Prelude (Char, placeholder)
import Text.Show (ShowS)
import Prelude (ReadS, error)

isControl :: Char -> Bool
isControl = placeholder

isSpace :: Char -> Bool
isSpace = placeholder

isLower :: Char -> Bool
isLower = placeholder

isUpper :: Char -> Bool
isUpper = placeholder

isAlpha :: Char -> Bool
isAlpha = placeholder

isAlphaNum :: Char -> Bool
isAlphaNum = placeholder

isPrint :: Char -> Bool
isPrint = placeholder

isDigit :: Char -> Bool
isDigit = placeholder

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

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned

generalCategory :: Char -> GeneralCategory
generalCategory = placeholder

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

showLitChar :: Char -> ShowS
showLitChar = placeholder

lexLitChar :: ReadS String
lexLitChar = placeholder

readLitChar :: ReadS Char
readLitChar = placeholder
