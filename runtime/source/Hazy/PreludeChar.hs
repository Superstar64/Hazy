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

instance Read GeneralCategory where
  readsPrec = placeholder

instance Show GeneralCategory where
  showsPrec = placeholder

isSpace :: Char -> Bool
isSpace = \case
  '\t' -> True
  '\n' -> True
  '\r' -> True
  '\f' -> True
  '\v' -> True
  char | Space <- generalCategory char -> True
  _ -> False

isAlpha :: Char -> Bool
isAlpha char = case generalCategory char of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  _ -> False

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

isAlphaNum :: Char -> Bool
isAlphaNum char = case generalCategory char of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  DecimalNumber -> True
  LetterNumber -> True
  OtherNumber -> True
  _ -> False

showLitChar :: Char -> ShowS
showLitChar char =
  if isAlphaNum char || char == ' '
    then \string -> char : string
    else \string -> '\\' : showInt (fromEnum char) string

readLitChar :: ReadS Char
readLitChar = \case
  '\\' : string -> do
    (code, string) <- readDec string
    [(toEnum code, string)]
  c : string -> [(c, string)]

lexLitChar :: ReadS String
lexLitChar = \case
  '\\' : string -> do
    (digits, string) <- lexDigits string
    [('\\' : digits, string)]
  char : string -> [(char : [], string)]
  _ -> []
