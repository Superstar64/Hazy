{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Scheme where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Constraint (Constraint)
import qualified Stage2.Tree.Constraint as Constraint
import Stage2.Tree.Type (Type)
import Stage2.Tree.Type as Type (anonymize)
import Stage2.Tree.TypePattern (TypePattern)
import qualified Stage2.Tree.TypePattern as TypePattern

data Scheme position stage scope = Scheme
  { startPosition :: !position,
    implicit :: !Bool,
    parameters :: !(Strict.Vector (TypePattern position stage scope)),
    constraints :: !(Strict.Vector (Constraint position stage scope)),
    result :: !(Type position stage (Local ':+ scope))
  }
  deriving (Show, Eq)

instance Shift (Scheme position stage) where
  shift = shiftDefault

instance Shift.Functor (Scheme position stage) where
  map category Scheme {startPosition, implicit, parameters, constraints, result} =
    Scheme
      { startPosition,
        implicit,
        parameters = Shift.map category <$> parameters,
        constraints = fmap (Shift.map category) constraints,
        result = Shift.map (Shift.Over category) result
      }

instance FreeTypeVariables (Scheme position) where
  freeTypeVariables target Scheme {constraints, result} =
    concat
      [ foldMap (freeTypeVariables target) constraints,
        freeTypeVariables (FreeVariables.Over target) result
      ]

anonymize :: Scheme position stage scope -> Scheme () stage scope
anonymize Scheme {implicit, parameters, constraints, result} =
  Scheme
    { startPosition = (),
      implicit,
      parameters = TypePattern.anonymize <$> parameters,
      constraints = Constraint.anonymize <$> constraints,
      result = Type.anonymize result
    }
