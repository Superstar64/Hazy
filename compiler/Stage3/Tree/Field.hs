module Stage3.Tree.Field where

import Stage1.Variable (Variable)
import Stage3.Tree.Entry (Entry)

data Field scope = Field
  { name :: !Variable,
    entry :: !(Entry scope)
  }
  deriving (Show)
