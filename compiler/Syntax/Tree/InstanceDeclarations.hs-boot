module Syntax.Tree.InstanceDeclarations where

import Syntax.Parser (Parser)
import Syntax.Position (Position)

data InstanceDeclarations position

instance (Show position) => Show (InstanceDeclarations position)

parse :: Parser (InstanceDeclarations Position)
