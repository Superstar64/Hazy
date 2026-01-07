module Stage1.Tree.TypeInfix where

import Stage1.FreeTypeVariables (FreeTypeVariables)
import Stage1.Parser (Parser)
import Stage1.Position (Position)
import Stage1.Printer (Printed)
import {-# SOURCE #-} Stage1.Tree.Type (Type)

data Infix position

instance (Show position) => Show (Infix position)

instance FreeTypeVariables Infix

parse :: Parser (Infix Position)
toType :: Infix Position -> Type Position
print :: Infix unit -> Printed
