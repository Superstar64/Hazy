module Semantic.Check.ClassInstance where

import Core.Instanciate (Instanciated)
import Core.Tree.Class (DefinitionF (..))
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Simple.MethodInfo (MethodInfo (..))
import qualified Semantic.Unify as Unify

type ClassInstance s scope =
  DefinitionF
    Instanciated
    (Unify.LogicalEvidence s scope)
    (Unify.Logical s scope)
    scope

info :: ClassInstance s scope -> MethodInfo scope
info Definition {constraintCount} = MethodInfo {constraintCount}

methodFunction :: ClassInstance s scope -> Int -> Unify.Forall s scope
methodFunction Definition {methods} index = methods Strict.Vector.! index
