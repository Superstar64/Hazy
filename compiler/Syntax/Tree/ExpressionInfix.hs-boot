module Syntax.Tree.ExpressionInfix where

import Syntax.Parser (Parser)
import Syntax.Position (Position)
import {-# SOURCE #-} Syntax.Tree.Expression (Expression)

data Infix position

instance (Show position) => Show (Infix position)

parse :: Parser (Infix Position)
parseLeftSection :: Parser (Expression Position)
toExpression :: Infix position -> Expression position
