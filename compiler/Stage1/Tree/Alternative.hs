-- |
-- Syntax tree for case alternatives
module Stage1.Tree.Alternative (Alternative (..), parse, parseMany) where

import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Parser (Parser, betweenBraces, sepEndBySemicolon, token)
import Stage1.Position (Position)
import Stage1.Tree.Pattern (Pattern)
import qualified Stage1.Tree.Pattern as Pattern
import Stage1.Tree.RightHandSide (RightHandSide)
import qualified Stage1.Tree.RightHandSide as RightHandSide

data Alternative position
  = -- |
    -- Case alternative
    --
    -- > case e of { x -> e }
    -- >             ^^^^^^
    Alternative !(Pattern position) !(RightHandSide position)
  deriving (Show)

parse :: Parser (Alternative Position)
parse = Alternative <$> Pattern.parse <*> RightHandSide.parse (token "->")

-- |
-- Parse multiple alternatives between braces
parseMany :: Parser (Strict.Vector (Alternative Position))
parseMany = betweenBraces (Strict.Vector.fromList <$> sepEndBySemicolon parse)
