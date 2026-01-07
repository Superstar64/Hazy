module Stage3.Tree.PatternField where

import {-# SOURCE #-} Stage3.Tree.Pattern (Pattern)

data Field scope = Field !Int !(Pattern scope)
  deriving (Show)
