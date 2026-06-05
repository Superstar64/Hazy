module Syntax.Tree.PatternInfix where

import Syntax.FreeVariables (TermBindingVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)
import {-# SOURCE #-} Syntax.Tree.Pattern (Pattern)

data Infix position

instance (Show position) => Show (Infix position)

instance TermBindingVariables Infix

toPattern :: Infix position -> Pattern position
parse :: Parser (Infix Position)
