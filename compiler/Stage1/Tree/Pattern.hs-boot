module Stage1.Tree.Pattern where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables)

data Pattern position

instance (Show position) => Show (Pattern position)

instance TermBindingVariables Pattern

parse :: Parser (Pattern Position)
