{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Instance where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Error (notClassMethod, patternInMethod)
import Order (orderListInt')
import Stage1.Position (Position)
import qualified Stage1.Tree.Constraint as Stage1 (Constraint)
import qualified Stage1.Tree.InstanceDeclaration as Stage1 (InstanceDeclaration (..))
import qualified Stage1.Tree.InstanceDeclarations as Stage1 (InstanceDeclarations (..))
import qualified Stage1.Tree.TypePattern as Stage1 (TypePattern)
import Stage1.Variable (Variable)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage2.Temporary.Partial.Definition as Partial (Definition (..), resolve)
import Stage2.Tree.Constraint (Constraint)
import qualified Stage2.Tree.Constraint as Constraint
import Stage2.Tree.Definition (Definition)
import qualified Stage2.Tree.Definition as Definition (merge)
import qualified Stage2.Tree.Scheme as Scheme
import Stage2.Tree.TypePattern (TypePattern)
import qualified Stage2.Tree.TypePattern as TypePattern

data Instance scope = Instance
  { startPosition :: !Position,
    prerequisites :: !(Strict.Vector (Constraint Position scope)),
    classPosition :: !Position,
    parameters :: !(Strict.Vector (TypePattern Position)),
    members :: !(Strict.Vector (Strict.Maybe (Definition (Local ':+ scope))))
  }
  deriving (Show)

instance Shift Instance where
  shift = shiftDefault

instance Shift.Functor Instance where
  map category Instance {startPosition, prerequisites, classPosition, parameters, members} =
    Instance
      { startPosition,
        prerequisites = fmap (Shift.map category) prerequisites,
        classPosition,
        parameters,
        members = fmap (fmap (Shift.map (Shift.Over category))) members
      }

resolve ::
  Context scope ->
  Position ->
  Strict.Vector (Stage1.Constraint Position) ->
  Position ->
  Strict.Vector (Stage1.TypePattern Position) ->
  Map Variable Int ->
  Stage1.InstanceDeclarations Position ->
  Instance scope
resolve
  context
  startPosition
  prerequisites
  classPosition
  parameters
  memberMethods
  Stage1.InstanceDeclarations {declarations}
    | parameters <- TypePattern.resolve <$> parameters,
      prerequisites <- fmap (Constraint.resolve (Scheme.augmentWith parameters context)) prerequisites =
        let members = orderListInt' combine (length memberMethods) members
              where
                combine (member : members) = Strict.Just $ Definition.merge (member :| members)
                combine [] = Strict.Nothing
                members = map member (toList declarations)
                member = \case
                  Stage1.Definition {startPosition, leftHandSide, rightHandSide} ->
                    case Partial.resolve
                      (patternInMethod startPosition)
                      (Scheme.augmentWith parameters context)
                      leftHandSide
                      rightHandSide of
                      Partial.Definition _ name function
                        | Just index <- Map.lookup name memberMethods -> (index, function)
                        | otherwise -> notClassMethod startPosition
         in Instance
              { startPosition,
                prerequisites,
                parameters,
                members,
                classPosition
              }
