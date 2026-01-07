module Stage3.Tree.Definition where

import Stage3.Tree.Function (Function)

data Definition scope
  = Alternative
      { definition :: !(Function scope),
        alternative :: !(Definition scope)
      }
  | Definition
      { definition :: !(Function scope)
      }
  deriving (Show)
