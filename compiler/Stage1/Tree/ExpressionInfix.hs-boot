module Stage1.Tree.ExpressionInfix where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)

data Infix position

instance (Show position) => Show (Infix position)

parse :: Parser (Infix Position)
parseLeftSection :: Parser (Expression Position)
toExpression :: Infix position -> Expression position
