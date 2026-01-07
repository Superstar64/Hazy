module Stage2.Temporary.Partial.More.GADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Temporary.Complete.GADTConstructor (GADTConstructor)
import Stage2.Tree.TypePattern (TypePattern)

data GADT scope = GADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position)),
    gadtConstructors :: !(Strict.Vector (GADTConstructor scope))
  }
