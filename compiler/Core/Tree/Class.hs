module Core.Tree.Class where

import Core.Instanciate (Normal)
import qualified Core.Substitute as Substitute
import Core.Tree.Combinators.Delay (Delay)
import Core.Tree.Constraint (Constraint)
import Core.Tree.Evidence (EvidenceF)
import Core.Tree.Forall (ForallOver)
import Core.Tree.MethodInfo (MethodInfo (..))
import Core.Tree.Type (Type, TypeF, (-#>))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    definition :: !(Definition (Local ':+ scope))
  }
  deriving (Show)

instance Shift0.Functor Class where
  map = Shift.mapDefault

instance Shift.Functor Class where
  map = Substitute.mapDefault

instance Substitute.Functor Class where
  map category Class {parameter, constraints, definition} =
    Class
      { parameter = Substitute.map category parameter,
        constraints = Substitute.map category <$> constraints,
        definition = Substitute.map (Substitute.Over category) definition
      }

type Definition = DefinitionF Normal Void Void

data DefinitionF instanciate logicalEvidence logicalType scope = Definition
  { typex :: !(Delay TypeF instanciate logicalType scope),
    evidence :: !(Delay EvidenceF instanciate logicalEvidence scope),
    methods :: !(Strict.Vector (ForallOver TypeF logicalType scope)),
    constraintCount :: !Int
  }
  deriving (Show)

instance Shift0.Functor (DefinitionF instanciate logicalEvidence logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (DefinitionF instanciate logicalEvidence logicalType) where
  map category Definition {typex, evidence, methods, constraintCount} =
    Definition
      { typex = Shift.map category typex,
        evidence = Shift.map category evidence,
        methods = Shift.map category <$> methods,
        constraintCount
      }

instance
  (logicalEvidence ~ Void, logicalType ~ Void) =>
  Substitute.Functor (DefinitionF instanciate logicalEvidence logicalType)
  where
  map category Definition {typex, evidence, methods, constraintCount} =
    Definition
      { typex = Substitute.map category typex,
        evidence = Substitute.map category evidence,
        methods = Substitute.map category <$> methods,
        constraintCount
      }

kind :: Class scope -> Type scope
kind Class {parameter} = parameter -#> Type.Constraint

info :: Class scope -> MethodInfo scope
info Class {constraints} = MethodInfo {constraintCount = length constraints}
