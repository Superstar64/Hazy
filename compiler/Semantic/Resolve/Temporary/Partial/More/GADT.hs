module Semantic.Resolve.Temporary.Partial.More.GADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Resolve.Temporary.Complete.GADTConstructor (GADTConstructor)
import Semantic.Stage (Resolve)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

data GADT scope = GADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    gadtConstructors :: !(Strict.Vector (GADTConstructor scope))
  }
