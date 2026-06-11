module Hazy.PreludeChar where

import Hazy
import Hazy.Helper
import Hazy.Prelude

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

instance Enum GeneralCategory where
  toEnum x | x >= 0 && x < 30 = primFromConstructorTag x
  fromEnum = primToConstructorTag

instance Eq GeneralCategory where
  (==) = enumEqual

instance Ord GeneralCategory where
  compare = enumCompare

instance Bounded GeneralCategory where
  minBound = UppercaseLetter
  maxBound = NotAssigned

isSpace :: Char -> Bool
isSpace = placeholder

isAlpha :: Char -> Bool
isAlpha = placeholder

isDigit :: Char -> Bool
isDigit = placeholder

isAlphaNum :: Char -> Bool
isAlphaNum = placeholder

showLitChar :: Char -> ShowS
showLitChar = placeholder

readLitChar :: ReadS Char
readLitChar = placeholder

lexLitChar :: ReadS String
lexLitChar = placeholder
