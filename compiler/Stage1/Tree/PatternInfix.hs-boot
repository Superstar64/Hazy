module Stage1.Tree.PatternInfix where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables)
import {-# SOURCE #-} Stage1.Tree.Pattern (Pattern)

data Infix position

instance (Show position) => Show (Infix position)

instance TermBindingVariables Infix

toPattern :: Infix position -> Pattern position
parse :: Parser (Infix Position)
