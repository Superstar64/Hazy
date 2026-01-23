module Stage4.Temporary.Definition where

import qualified Stage3.Tree.Definition as Stage3
import Stage4.Temporary.Function (Function)
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)

data Definition scope
  = Alternative
      { definition :: !(Function scope),
        alternative :: !(Definition scope)
      }
  | Definition
      { definition :: !(Function scope)
      }

instance Semigroup (Definition scope)

simplify :: Stage3.Definition scope -> Definition scope
desugar :: Definition scope -> Expression scope
