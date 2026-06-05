module Semantic.Check.ClassInstance where

import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Simple.MethodInfo (MethodInfo (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify

data ClassInstance s scope = ClassInstance
  { typex :: !(Unify.Type s scope),
    evidence :: !(Unify.Evidence s scope),
    methods :: !(Strict.Vector (Unify.Scheme s scope)),
    constraintCount :: !Int
  }

info :: ClassInstance s scope -> MethodInfo scope
info ClassInstance {constraintCount} = MethodInfo {constraintCount}

methodFunction :: ClassInstance s scope -> Int -> Unify.Scheme s scope
methodFunction ClassInstance {methods} index = methods Strict.Vector.! index
