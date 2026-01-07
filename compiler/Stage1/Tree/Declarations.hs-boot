module Stage1.Tree.Declarations where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables)

data Declarations position

instance (Show position) => Show (Declarations position)

instance TermBindingVariables Declarations

parse :: Parser (Declarations Position)
empty :: Declarations position
