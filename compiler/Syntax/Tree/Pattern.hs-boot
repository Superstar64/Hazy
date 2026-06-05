module Syntax.Tree.Pattern where

import Syntax.FreeVariables (TermBindingVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)

data Pattern position

instance (Show position) => Show (Pattern position)

instance TermBindingVariables Pattern

parse :: Parser (Pattern Position)
