module Stage1.Tree.Type where

import Stage1.FreeTypeVariables (FreeTypeVariables)
import Stage1.Parser (Parser)
import Stage1.Position (Position)

data Type position

instance (Show position) => Show (Type position)

instance FreeTypeVariables Type

parse3 :: Parser (Type Position)
