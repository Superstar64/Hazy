module Syntax.Tree.TypeInfix where

import Syntax.FreeVariables (FreeTypeVariables)
import Syntax.Parser (Parser)
import Syntax.Position (Position)
import Syntax.Printer (Printed)
import {-# SOURCE #-} Syntax.Tree.Type (Type)

data Infix position

instance (Show position) => Show (Infix position)

instance FreeTypeVariables Infix

parse :: Parser (Infix Position)
toType :: Infix Position -> Type Position
print :: Infix unit -> Printed
