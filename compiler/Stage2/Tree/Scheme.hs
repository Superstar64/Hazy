{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Scheme where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.FreeTypeVariables (FreeTypeVariables (..))
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Scheme as Stage1 (Scheme (..))
import qualified Stage2.Index.Local as Local
import Stage2.Resolve.Context (Context (..))
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Constraint (Constraint)
import qualified Stage2.Tree.Constraint as Constraint
import Stage2.Tree.Type (Type)
import Stage2.Tree.Type as Type (anonymize, resolve)
import Stage2.Tree.TypePattern (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as TypePattern

data Scheme position scope = Scheme
  { startPosition :: !position,
    implicit :: !Bool,
    parameters :: !(Strict.Vector (TypePattern position)),
    constraints :: !(Strict.Vector (Constraint position scope)),
    result :: !(Type position (Local ':+ scope))
  }
  deriving (Show, Eq)

instance Shift (Scheme position) where
  shift = shiftDefault

instance Shift.Functor (Scheme position) where
  map category Scheme {startPosition, implicit, parameters, constraints, result} =
    Scheme
      { startPosition,
        implicit,
        parameters,
        constraints = fmap (Shift.map category) constraints,
        result = Shift.map (Shift.Over category) result
      }

anonymize :: Scheme position scope -> Scheme () scope
anonymize Scheme {implicit, parameters, constraints, result} =
  Scheme
    { startPosition = (),
      implicit,
      parameters = TypePattern.anonymize <$> parameters,
      constraints = Constraint.anonymize <$> constraints,
      result = Type.anonymize result
    }

augment :: Scheme Position scope -> Context scope -> Context (Local ':+ scope)
augment Scheme {implicit = False, parameters} = augmentWith parameters
augment Scheme {implicit = True} = augmentWith Strict.Vector.empty

augmentWith :: Strict.Vector (TypePattern position) -> Context scopes -> Context (Local ':+ scopes)
augmentWith parameters context = case shift context of
  context@Context {localTypes} -> context {localTypes = Map.fromList (zip (toList variables) indexes) <> localTypes}
  where
    variables = TypePattern.name <$> parameters
    indexes = Local.Local <$> [0 ..]

resolve :: Context scope -> Stage1.Scheme Position -> Scheme Position scope
resolve context (Stage1.Explicit {startPosition, parameters, constraints, result})
  | parameters <- TypePattern.resolve <$> parameters,
    context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = fmap (Constraint.resolve context) constraints,
          result = Type.resolve context result
        }
  where
    implicit = False
resolve context Stage1.Implicit {startPosition, constraints, result}
  | context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = fmap (Constraint.resolve context) constraints,
          result = Type.resolve context result
        }
  where
    implicit = True
    parameters = fabricate <$> names
    fabricate name = TypePattern {position = startPosition, name}
    names = Strict.Vector.fromList $ nubOrd $ filter (`Map.notMember` localTypes context) free
    free = map (\(_ :@ name) -> name) $ foldMap freeTypeVariables constraints <> freeTypeVariables result
