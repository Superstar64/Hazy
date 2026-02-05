module Stage3.Tree.Entry where

import Stage3.Tree.Type (Type)

data Entry scope = Entry
  { entry :: !(Type scope),
    strict :: !Bool
  }
  deriving (Show)
