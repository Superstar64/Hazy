-- |
-- Parser syntax tree for operator fixity
module Stage1.Tree.Fixity where

import Data.Maybe (fromMaybe)
import Stage1.Parser (Parser, integer, optional)
import Stage1.Tree.Associativity (Associativity)
import qualified Stage1.Tree.Associativity as Associativity

data Fixity
  = -- |
    -- > infix 9 `x`
    -- > ^^^^^^^
    Fixity !Associativity !Int
  deriving (Show, Read)

parse :: Parser Fixity
parse = Fixity <$> Associativity.parse <*> (fromMaybe 9 <$> optional (fromInteger <$> integer))
