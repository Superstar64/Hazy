{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Scheme where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Constraints (Constraints)
import qualified Semantic.Tree.Constraints as Constraints
import Semantic.Tree.Type (Type)
import Semantic.Tree.Type as Type (anonymize)
import Semantic.Tree.TypePattern (TypePattern)
import qualified Semantic.Tree.TypePattern as TypePattern

data Scheme position stage scope = Scheme
  { startPosition :: !position,
    implicit :: !Bool,
    parameters :: !(Strict.Vector (TypePattern position stage scope)),
    constraints :: !(Constraints position stage scope),
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
        constraints = Shift.map category constraints,
        result = Shift.map (Shift.Over category) result
      }

instance FreeTypeVariables (Scheme position) where
  freeTypeVariables target Scheme {constraints, result} =
    concat
      [ freeTypeVariables target constraints,
        freeTypeVariables (FreeVariables.Over target) result
      ]

anonymize :: Scheme position stage scope -> Scheme () stage scope
anonymize Scheme {implicit, parameters, constraints, result} =
  Scheme
    { startPosition = (),
      implicit,
      parameters = TypePattern.anonymize <$> parameters,
      constraints = Constraints.anonymize constraints,
      result = Type.anonymize result
    }
