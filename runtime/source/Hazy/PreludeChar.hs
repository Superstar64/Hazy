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
  readsPrec _ = readParen False $ \string -> do
    (token, string) <- lex string
    case token of
      "UppercaseLetter" -> [(UppercaseLetter, string)]
      "LowercaseLetter" -> [(LowercaseLetter, string)]
      "TitlecaseLetter" -> [(TitlecaseLetter, string)]
      "ModifierLetter" -> [(ModifierLetter, string)]
      "OtherLetter" -> [(OtherLetter, string)]
      "NonSpacingMark" -> [(NonSpacingMark, string)]
      "SpacingCombiningMark" -> [(SpacingCombiningMark, string)]
      "EnclosingMark" -> [(EnclosingMark, string)]
      "DecimalNumber" -> [(DecimalNumber, string)]
      "LetterNumber" -> [(LetterNumber, string)]
      "OtherNumber" -> [(OtherNumber, string)]
      "ConnectorPunctuation" -> [(ConnectorPunctuation, string)]
      "DashPunctuation" -> [(DashPunctuation, string)]
      "OpenPunctuation" -> [(OpenPunctuation, string)]
      "ClosePunctuation" -> [(ClosePunctuation, string)]
      "InitialQuote" -> [(InitialQuote, string)]
      "FinalQuote" -> [(FinalQuote, string)]
      "OtherPunctuation" -> [(OtherPunctuation, string)]
      "MathSymbol" -> [(MathSymbol, string)]
      "CurrencySymbol" -> [(CurrencySymbol, string)]
      "ModifierSymbol" -> [(ModifierSymbol, string)]
      "OtherSymbol" -> [(OtherSymbol, string)]
      "Space" -> [(Space, string)]
      "LineSeparator" -> [(LineSeparator, string)]
      "ParagraphSeparator" -> [(ParagraphSeparator, string)]
      "Control" -> [(Control, string)]
      "Format" -> [(Format, string)]
      "Surrogate" -> [(Surrogate, string)]
      "PrivateUse" -> [(PrivateUse, string)]
      "NotAssigned" -> [(NotAssigned, string)]
      _ -> []

instance Show GeneralCategory where
  showsPrec _ = \case
    UppercaseLetter -> showString "UppercaseLetter"
    LowercaseLetter -> showString "LowercaseLetter"
    TitlecaseLetter -> showString "TitlecaseLetter"
    ModifierLetter -> showString "ModifierLetter"
    OtherLetter -> showString "OtherLetter"
    NonSpacingMark -> showString "NonSpacingMark"
    SpacingCombiningMark -> showString "SpacingCombiningMark"
    EnclosingMark -> showString "EnclosingMark"
    DecimalNumber -> showString "DecimalNumber"
    LetterNumber -> showString "LetterNumber"
    OtherNumber -> showString "OtherNumber"
    ConnectorPunctuation -> showString "ConnectorPunctuation"
    DashPunctuation -> showString "DashPunctuation"
    OpenPunctuation -> showString "OpenPunctuation"
    ClosePunctuation -> showString "ClosePunctuation"
    InitialQuote -> showString "InitialQuote"
    FinalQuote -> showString "FinalQuote"
    OtherPunctuation -> showString "OtherPunctuation"
    MathSymbol -> showString "MathSymbol"
    CurrencySymbol -> showString "CurrencySymbol"
    ModifierSymbol -> showString "ModifierSymbol"
    OtherSymbol -> showString "OtherSymbol"
    Space -> showString "Space"
    LineSeparator -> showString "LineSeparator"
    ParagraphSeparator -> showString "ParagraphSeparator"
    Control -> showString "Control"
    Format -> showString "Format"
    Surrogate -> showString "Surrogate"
    PrivateUse -> showString "PrivateUse"
    NotAssigned -> showString "NotAssigned"

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
