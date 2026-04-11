module Stage1.Tree.ClassDeclarations where

import Stage1.FreeVariables (TermBindingVariables)
import Stage1.Parser (Parser)
import Stage1.Position (Position)

data ClassDeclarations position

instance (Show position) => Show (ClassDeclarations position)

instance TermBindingVariables ClassDeclarations

parse :: Parser (ClassDeclarations Position)
