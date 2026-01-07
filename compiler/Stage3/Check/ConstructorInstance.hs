module Stage3.Check.ConstructorInstance where

import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} qualified Stage3.Unify as Unify

newtype ConstructorInstance s scope = ConstructorInstance
  { entries :: Strict.Vector (Unify.Type s scope)
  }
