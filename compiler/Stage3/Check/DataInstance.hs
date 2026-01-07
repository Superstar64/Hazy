module Stage3.Check.DataInstance where

import qualified Data.Vector.Strict as Strict
import Stage2.Tree.Selector (Selector)
import Stage3.Check.ConstructorInstance (ConstructorInstance)
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data DataInstance s scope = DataInstance
  { types :: !(Strict.Vector (Unify.Type s scope)),
    constructors :: !(Strict.Vector (ConstructorInstance s scope)),
    selectors :: !(Strict.Vector Selector)
  }
