module Stage1.Tree.ClassDeclarations where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import Stage1.TermBindingVariables (TermBindingVariables)

data ClassDeclarations position

instance (Show position) => Show (ClassDeclarations position)

instance TermBindingVariables ClassDeclarations

parse :: Parser (ClassDeclarations Position)
