{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Syntax tree for case alternatives
module Syntax.Tree.Alternative (Alternative (..), parse, parseMany) where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Syntax.Parser (Parser, betweenBraces, sepEndBySemicolon, token)
import Syntax.Position (Position)
import Syntax.Tree.Pattern (Pattern)
import qualified Syntax.Tree.Pattern as Pattern
import Syntax.Tree.RightHandSide (RightHandSide)
import qualified Syntax.Tree.RightHandSide as RightHandSide

data Alternative position
  = -- |
    -- Case alternative
    --
    -- > case e of { x -> e }
    -- >             ^^^^^^
    Alternative
    { parameter :: !(Pattern position),
      rightHandSide :: !(RightHandSide position)
    }
  deriving (Show)

parse :: Parser (Alternative Position)
parse = alternative <$> Pattern.parse <*> RightHandSide.parse (token "->")
  where
    alternative parameter rightHandSide = Alternative {parameter, rightHandSide}

-- |
-- Parse multiple alternatives between braces
parseMany :: Parser (Strict.Vector (Alternative Position))
parseMany = betweenBraces (Strict.Vector.fromList <$> sepEndBySemicolon parse)
