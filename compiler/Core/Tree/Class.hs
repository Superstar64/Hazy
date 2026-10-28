module Core.Tree.Class where

import Core.Instanciate (Instanciated, Normal)
import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (Constraint)
import Core.Tree.Evidence (EvidenceF)
import Core.Tree.Forall (ForallOver)
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
  { typex :: !(TypeInfo instanciate logicalType scope),
    evidence :: !(EvidenceInfo instanciate logicalEvidence logicalType scope),
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

data TypeInfo instanciate logicalType scope where
  TypeInfo :: !(TypeF logicalType scope) -> TypeInfo Instanciated logicalType scope
  TypeNormal :: TypeInfo Normal logicalType scope

instance (Show logicalType) => Show (TypeInfo instanciate logicalType scope) where
  showsPrec k = \case
    TypeInfo typex -> showParen (k > 10) $ showString "TypeInfo " . showsPrec 11 typex
    TypeNormal -> showString "TypeNormal"

instance Shift0.Functor (TypeInfo instanciate logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (TypeInfo instanciate logicalType) where
  map category = \case
    TypeInfo typex -> TypeInfo (Shift.map category typex)
    TypeNormal -> TypeNormal

instance (logicalType ~ Void) => Substitute.Functor (TypeInfo instanciate logicalType) where
  map category = \case
    TypeInfo typex -> TypeInfo (Substitute.map category typex)
    TypeNormal -> TypeNormal

data EvidenceInfo instanciate logicalEvidence logicalType scope where
  EvidenceInfo ::
    !(EvidenceF logicalEvidence logicalType scope) ->
    EvidenceInfo Instanciated logicalEvidence logicalType scope
  EvidenceNormal :: EvidenceInfo Normal logicalEvidence logicalType scope

instance
  (Show logicalEvidence, Show logicalType) =>
  Show (EvidenceInfo instanciate logicalEvidence logicalType scope)
  where
  showsPrec k = \case
    EvidenceInfo typex -> showParen (k > 10) $ showString "EvidenceInfo " . showsPrec 11 typex
    EvidenceNormal -> showString "EvidenceNormal"

instance Shift0.Functor (EvidenceInfo instanciate logicalEvidence logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (EvidenceInfo instanciate logicalEvidence logicalType) where
  map category = \case
    EvidenceInfo typex -> EvidenceInfo (Shift.map category typex)
    EvidenceNormal -> EvidenceNormal

instance
  (logicalType ~ Void, logicalEvidence ~ Void) =>
  Substitute.Functor (EvidenceInfo instanciate logicalEvidence logicalType)
  where
  map category = \case
    EvidenceInfo typex -> EvidenceInfo (Substitute.map category typex)
    EvidenceNormal -> EvidenceNormal

kind :: Class scope -> Type scope
kind Class {parameter} = parameter -#> Type.Constraint

info :: Class scope -> Int
info Class {constraints} = length constraints
