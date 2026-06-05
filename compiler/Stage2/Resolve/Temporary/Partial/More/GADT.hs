module Stage2.Resolve.Temporary.Partial.More.GADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Stage (Resolve)
import Stage2.Resolve.Temporary.Complete.GADTConstructor (GADTConstructor)
import Stage2.Tree.TypePattern (TypePattern)

data GADT scope = GADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    gadtConstructors :: !(Strict.Vector (GADTConstructor scope))
  }
