{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for operator fixity
module Syntax.Tree.Fixity where

import Data.Maybe (fromMaybe)
import Syntax.Parser (Parser, integer, optional)
import Syntax.Tree.Associativity (Associativity)
import qualified Syntax.Tree.Associativity as Associativity

data Fixity
  = -- |
    -- > infix 9 `x`
    -- > ^^^^^^^
    Fixity
    { associativity :: Associativity,
      precedence :: !Int
    }
  deriving (Show, Read)

parse :: Parser Fixity
parse = fixity <$> Associativity.parse <*> (fromMaybe 9 <$> optional (fromInteger <$> integer))
  where
    fixity associativity precedence = Fixity {associativity, precedence}
