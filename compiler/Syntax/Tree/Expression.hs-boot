module Syntax.Tree.Expression where

import Syntax.Parser (Parser)
import Syntax.Position (Position)

data Expression position

instance (Show position) => Show (Expression position)

parse :: Parser (Expression Position)
