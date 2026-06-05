{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Instance where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import Error (notClassMethod, patternInMethod)
import Order (orderListInt')
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Constraint as Constraint
import qualified Semantic.Resolve.Go.Scheme as Scheme
import qualified Semantic.Resolve.Go.TypePattern as TypePattern
import qualified Semantic.Resolve.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition as Definition (merge)
import Semantic.Tree.Instance (Instance (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraint as Syntax (Constraint)
import qualified Syntax.Tree.InstanceDeclaration as Syntax (InstanceDeclaration (..))
import qualified Syntax.Tree.InstanceDeclarations as Syntax (InstanceDeclarations (..))
import qualified Syntax.Tree.TypePattern as Syntax (TypePattern)
import Syntax.Variable (Variable)

resolve ::
  Context scope ->
  Position ->
  Strict.Vector (Syntax.Constraint Position) ->
  Position ->
  Strict.Vector (Syntax.TypePattern Position) ->
  Map Variable Int ->
  Syntax.InstanceDeclarations Position ->
  Instance Normal Resolve scope
resolve
  context
  startPosition
  prerequisites
  classPosition
  parameters
  memberMethods
  Syntax.InstanceDeclarations {declarations}
    | parameters <- TypePattern.resolve <$> parameters,
      prerequisites <- fmap (Constraint.resolve (Scheme.augmentWith parameters context)) prerequisites =
        let members = orderListInt' combine (length memberMethods) members
              where
                combine (member : members) = Definition $ Resolve $ Definition.merge (member :| members)
                combine [] = Default {base = Inferred, self = Inferred, defaultx = Inferred}
                members = map member (toList declarations)
                member = \case
                  Syntax.Definition {startPosition, leftHandSide, rightHandSide} ->
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
