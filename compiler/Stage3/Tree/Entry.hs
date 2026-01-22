module Stage3.Tree.Entry where

import Stage3.Tree.Type (Type)
import qualified Stage4.Tree.Type as Simple

data Entry scope = Entry
  { entry :: !(Type scope),
    entry' :: !(Simple.Type scope),
    strict :: !Bool
  }
  deriving (Show)
