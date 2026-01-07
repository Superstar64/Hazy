module Stage3.Check.ClassInstance where

import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data ClassInstance s scope = ClassInstance
  { typex :: !(Unify.Type s scope),
    evidence :: !(Unify.Evidence s scope),
    methods :: !(Strict.Vector (Unify.Scheme s scope))
  }
