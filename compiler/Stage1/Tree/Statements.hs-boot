module Stage1.Tree.Statements where

import Stage1.Parser (Parser)
import Stage1.Position (Position)
import {-# SOURCE #-} Stage1.Tree.Expression (Expression)

data Statements position

instance (Show position) => Show (Statements position)

parseComprehension :: Parser (Expression Position -> Statements Position)
parseDo :: Parser (Statements Position)
