module Stage2.Resolve.Temporary.Partial.More.ADT where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Resolve)
import Stage2.Resolve.Temporary.Complete.Constructor (Constructor)
import Stage2.Resolve.Temporary.Complete.Selector (Selector)
import Stage2.Tree.TypePattern (TypePattern)

data ADT scope = ADT
  { brand :: !Brand,
    parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
    selectors :: !(Strict.Vector Selector)
  }
