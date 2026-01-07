-- |
-- Parser syntax tree for module import symbol groups
module Stage1.Tree.ImportSymbols where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser
  ( Parser,
    asum,
    betweenParens,
    sepEndByComma,
    token,
  )
import Stage1.Tree.ImportSymbol (Symbol)
import qualified Stage1.Tree.ImportSymbol as ImportSymbol

data Symbols
  = -- |
    -- > import M ( x )
    -- >          ^^^^^
    Symbols !(Strict.Vector Symbol)
  | -- |
    -- > import M hiding ( x )
    -- >          ^^^^^^^^^^^^
    Hiding !(Strict.Vector Symbol)
  | All
  deriving (Show)

parse :: Parser Symbols
parse =
  asum
    [ Symbols <$> betweenParens parseMany,
      Hiding <$> (token "hiding" *> betweenParens parseMany),
      pure All
    ]
  where
    parseMany :: Parser (Strict.Vector Symbol)
    parseMany = Strict.Vector.fromList <$> sepEndByComma ImportSymbol.parse
