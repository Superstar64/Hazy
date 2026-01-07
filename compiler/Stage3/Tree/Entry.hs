module Stage3.Tree.Entry where

import qualified Stage3.Simple.Type as Simple
import Stage3.Tree.Type (Type)

data Entry scope = Entry
  { entry :: !(Type scope),
    entry' :: !(Simple.Type scope),
    strict :: !Bool
  }
  deriving (Show)
