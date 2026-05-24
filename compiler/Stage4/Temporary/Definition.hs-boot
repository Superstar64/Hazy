module Stage4.Temporary.Definition where

import Stage2.Layout (Normal)
import Stage2.Stage (Check)
import qualified Stage2.Tree.Definition as Stage3
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

simplify :: Stage3.Definition Normal Check scope -> Definition scope
desugar :: Definition scope -> Expression scope
