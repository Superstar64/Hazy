module Syntax.StringLiteral (StringLiteral, pack, unpack) where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector

-- |
-- String literals can contain surrigate characters. So using `Text` for them
-- isn't an option.
newtype StringLiteral = StringLiteral {runString :: Strict.Vector Char}
  deriving (Eq, Ord)

instance Show StringLiteral where
  show = show . unpack

pack :: String -> StringLiteral
pack = StringLiteral . Strict.Vector.fromList

unpack :: StringLiteral -> String
unpack = Strict.Vector.toList . runString
