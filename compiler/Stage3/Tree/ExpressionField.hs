module Stage3.Tree.ExpressionField where

import {-# SOURCE #-} Stage3.Tree.Expression (Expression)

data Field scope = Field
  { index :: !Int,
    expression :: !(Expression scope)
  }
  deriving (Show)
