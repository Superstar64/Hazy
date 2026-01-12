module Stage4.Temporary.Definition where

import {-# SOURCE #-} Stage4.Temporary.Expression (Expression)
import Stage4.Temporary.Function (Function)

data Definition scope
  = Alternative
      { definition :: !(Function scope),
        alternative :: !(Definition scope)
      }
  | Definition
      { definition :: !(Function scope)
      }

instance Semigroup (Definition scope)

desugar :: Definition scope -> Expression scope
