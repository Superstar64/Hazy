module Syntax.Tree.ClassDeclarations where

import Syntax.FreeVariables (TermBindingVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)

data ClassDeclarations position

instance (Show position) => Show (ClassDeclarations position)

instance TermBindingVariables ClassDeclarations

parse :: Parser (ClassDeclarations Position)
