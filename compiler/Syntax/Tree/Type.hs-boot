module Syntax.Tree.Type where

import Syntax.FreeVariables (FreeTypeVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)

data Type position

instance (Show position) => Show (Type position)

instance FreeTypeVariables Type

parse3 :: Parser (Type Position)
