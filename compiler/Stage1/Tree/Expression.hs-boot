module Stage1.Tree.Expression where

import Stage1.Parser (Parser)
import Stage1.Position (Position)

data Expression position

instance (Show position) => Show (Expression position)

parse :: Parser (Expression Position)
