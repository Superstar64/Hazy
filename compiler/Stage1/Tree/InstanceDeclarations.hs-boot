module Stage1.Tree.InstanceDeclarations where

import Stage1.Parser (Parser)
import Stage1.Position (Position)

data InstanceDeclarations position

instance (Show position) => Show (InstanceDeclarations position)

parse :: Parser (InstanceDeclarations Position)
