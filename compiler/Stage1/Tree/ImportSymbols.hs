{-# LANGUAGE_HAZY UnorderedRecords #-}

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
    Symbols {symbols :: !(Strict.Vector Symbol)}
  | -- |
    -- > import M hiding ( x )
    -- >          ^^^^^^^^^^^^
    Hiding {symbols :: !(Strict.Vector Symbol)}
  | All
  deriving (Show)

parse :: Parser Symbols
parse =
  asum
    [ symbols <$> betweenParens parseMany,
      hiding <$> (token "hiding" *> betweenParens parseMany),
      pure All
    ]
  where
    symbols symbols = Symbols {symbols}
    hiding symbols = Hiding {symbols}
    parseMany :: Parser (Strict.Vector Symbol)
    parseMany = Strict.Vector.fromList <$> sepEndByComma ImportSymbol.parse
