module Javascript.Tree.Expression where

import qualified Javascript.Printer as Printer

data Expression

class Print ast

print :: (Print ast) => Expression -> ast

instance Print (Printer.AssignmentExpression inx yield await)
