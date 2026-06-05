module Core.Temporary.Definition where

import Core.Temporary.Function (Function)
import {-# SOURCE #-} Core.Tree.Expression (Expression)
import Semantic.Layout (Normal)
import Semantic.Stage (Check)
import qualified Semantic.Tree.Definition as Semantic

data Definition scope
  = Alternative
      { definition :: !(Function scope),
        alternative :: !(Definition scope)
      }
  | Definition
      { definition :: !(Function scope)
      }

instance Semigroup (Definition scope)

simplify :: Semantic.Definition Normal Check scope -> Definition scope
desugar :: Definition scope -> Expression scope
