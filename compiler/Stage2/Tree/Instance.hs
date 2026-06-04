{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Instance where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Connect (Connect (..))
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.MethodConcrete (MethodConcrete (..))
import Stage2.Tree.TypePattern (TypePattern)
import qualified Stage4.Tree.Evidence as Simple (Evidence)

data Instance layout stage scope = Instance
  { startPosition, classPosition :: !Position,
    parameters :: !(Strict.Vector (TypePattern Position stage scope)),
    prerequisites :: !(Strict.Vector (Constraint Position stage scope)),
    evidence :: !(Inferred Evidence stage scope),
    members :: !(Strict.Vector (MethodConcrete layout stage scope))
  }
  deriving (Show)

instance Shift (Instance layout stage) where
  shift = shiftDefault

instance Shift.Functor (Instance layout stage) where
  map category Instance {startPosition, prerequisites, classPosition, parameters, members, evidence} =
    Instance
      { startPosition,
        prerequisites = fmap (Shift.map category) prerequisites,
        classPosition,
        parameters = Shift.map category <$> parameters,
        members = fmap (Shift.map category) members,
        evidence = Shift.map category evidence
      }

instance Connect Instance where
  connect Instance {startPosition, prerequisites, classPosition, parameters, members} =
    Instance
      { startPosition,
        prerequisites,
        classPosition,
        parameters,
        members = connect <$> members,
        evidence = Inferred
      }
  seperate
    Instance
      { startPosition,
        prerequisites,
        classPosition,
        parameters,
        members,
        evidence
      } =
      Instance
        { startPosition,
          prerequisites,
          classPosition,
          parameters,
          members = seperate <$> members,
          evidence
        }

newtype Evidence scope = Evidence (Strict.Vector (Simple.Evidence (Local ':+ scope)))
  deriving (Show)

instance Scope.Show Evidence where
  showsPrec = showsPrec

instance Shift Evidence where
  shift = shiftDefault

instance Shift.Functor Evidence where
  map category (Evidence evidence) = Evidence (Shift.map (Shift.Over category) <$> evidence)
