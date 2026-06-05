{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Instance where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import Error (notClassMethod, patternInMethod)
import Order (orderListInt')
import Stage1.Position (Position)
import qualified Stage1.Tree.Constraint as Stage1 (Constraint)
import qualified Stage1.Tree.InstanceDeclaration as Stage1 (InstanceDeclaration (..))
import qualified Stage1.Tree.InstanceDeclarations as Stage1 (InstanceDeclarations (..))
import qualified Stage1.Tree.TypePattern as Stage1 (TypePattern)
import Stage1.Variable (Variable)
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Resolve.Go.Constraint as Constraint
import qualified Stage2.Resolve.Go.Scheme as Scheme
import qualified Stage2.Resolve.Go.TypePattern as TypePattern
import Stage2.Stage (Resolve)
import qualified Stage2.Resolve.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import Stage2.Tree.Combinators.Implicit (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Definition as Definition (merge)
import Stage2.Tree.Instance (Instance (..))
import Stage2.Tree.MethodConcrete (MethodConcrete (..))

resolve ::
  Context scope ->
  Position ->
  Strict.Vector (Stage1.Constraint Position) ->
  Position ->
  Strict.Vector (Stage1.TypePattern Position) ->
  Map Variable Int ->
  Stage1.InstanceDeclarations Position ->
  Instance Normal Resolve scope
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
                combine (member : members) = Definition $ Resolve $ Definition.merge (member :| members)
                combine [] = Default {base = Inferred, self = Inferred, defaultx = Inferred}
                members = map member (toList declarations)
                member = \case
                  Stage1.Definition {startPosition, leftHandSide, rightHandSide} ->
                    case Complete.resolve
                      (patternInMethod startPosition)
                      (Scheme.augmentWith parameters context)
                      leftHandSide
                      rightHandSide of
                      Complete.Definition _ name function
                        | Just index <- Map.lookup name memberMethods -> (index, function)
                        | otherwise -> notClassMethod startPosition
         in Instance
              { startPosition,
                prerequisites,
                parameters,
                members,
                classPosition,
                evidence = Inferred
              }
