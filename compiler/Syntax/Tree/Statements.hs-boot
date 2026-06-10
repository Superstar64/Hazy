module Syntax.Tree.Statements where

import Syntax.Parser (Parser)
import Syntax.Position (Position)
import {-# SOURCE #-} Syntax.Tree.Expression (Expression)

data Statements position

instance (Show position) => Show (Statements position)

parseComprehension :: Parser (Position -> Expression Position -> Statements Position)
parseDo :: Parser (Statements Position)
