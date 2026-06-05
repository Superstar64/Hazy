module Syntax.Tree.Declarations where

import Syntax.FreeVariables (TermBindingVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)

data Declarations position

instance (Show position) => Show (Declarations position)

instance TermBindingVariables Declarations

parse :: Parser (Declarations Position)
empty :: Declarations position
