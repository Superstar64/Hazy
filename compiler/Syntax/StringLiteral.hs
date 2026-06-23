module Syntax.StringLiteral (StringLiteral, pack, unpack) where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text

-- |
-- String literals can contain surrigate characters. So using plain `Text` for
-- them isn't an option.
data StringLiteral
  = String !Text
  | Surrogate !Text !Char !StringLiteral
  deriving (Eq, Ord)

instance Show StringLiteral where
  show = show . unpack

pack :: String -> StringLiteral
pack string = case break surrogate string of
  (normal, []) -> String (Text.pack normal)
  (normal, escape : remainder) -> Surrogate (Text.pack normal) escape (pack remainder)
  where
    surrogate char = case Char.generalCategory char of
      Char.Surrogate -> True
      _ -> False

unpack :: StringLiteral -> String
unpack = \case
  String string -> Text.unpack string
  Surrogate string char next -> Text.unpack string ++ char : unpack next
