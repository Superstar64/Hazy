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
import Hazy (Char)
import Text.Show (ShowS)
import Prelude (ReadS, error)

isControl :: Char -> Bool
isControl = error "todo"

isSpace :: Char -> Bool
isSpace = error "todo"

isLower :: Char -> Bool
isLower = error "todo"

isUpper :: Char -> Bool
isUpper = error "todo"

isAlpha :: Char -> Bool
isAlpha = error "todo"

isAlphaNum :: Char -> Bool
isAlphaNum = error "todo"

isPrint :: Char -> Bool
isPrint = error "todo"

isDigit :: Char -> Bool
isDigit = error "todo"

isOctDigit :: Char -> Bool
isOctDigit = error "todo"

isHexDigit :: Char -> Bool
isHexDigit = error "todo"

isLetter :: Char -> Bool
isLetter = error "todo"

isMark :: Char -> Bool
isMark = error "todo"

isNumber :: Char -> Bool
isNumber = error "todo"

isPunctuation :: Char -> Bool
isPunctuation = error "todo"

isSymbol :: Char -> Bool
isSymbol = error "todo"

isSeparator :: Char -> Bool
isSeparator = error "todo"

isAscii :: Char -> Bool
isAscii = error "todo"

isLatin1 :: Char -> Bool
isLatin1 = error "todo"

isAsciiUpper :: Char -> Bool
isAsciiUpper = error "todo"

isAsciiLower :: Char -> Bool
isAsciiLower = error "todo"

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
generalCategory = error "todo"

toUpper :: Char -> Char
toUpper = error "todo"

toLower :: Char -> Char
toLower = error "todo"

toTitle :: Char -> Char
toTitle = error "todo"

digitToInt :: Char -> Int
digitToInt = error "todo"

intToDigit :: Int -> Char
intToDigit = error "todo"

ord :: Char -> Int
ord = error "todo"

chr :: Int -> Char
chr = error "todo"

showLitChar :: Char -> ShowS
showLitChar = error "todo"

lexLitChar :: ReadS String
lexLitChar = error "todo"

readLitChar :: ReadS Char
readLitChar = error "todo"
