module Stage1.Tree.Pattern where

import Stage1.FreeVariables (TermBindingVariables)
import Stage1.Parser (Parser)
import Stage1.Position (Position)

data Pattern position

instance (Show position) => Show (Pattern position)

instance TermBindingVariables Pattern

parse :: Parser (Pattern Position)
