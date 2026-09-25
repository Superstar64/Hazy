{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Instance where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (notClassMethod, patternInMethod)
import Order (orderListInt')
import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Constraints as Constraints
import qualified Semantic.Resolve.Go.Scheme as Scheme
import qualified Semantic.Resolve.Go.TypePattern as TypePattern
import qualified Semantic.Resolve.Temporary.Complete.Definition as Complete (Definition (..), resolve)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Definition as Definition (merge)
import Semantic.Tree.Instance (Instance (..))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (Header (..), InstanceDefinition2 (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Constraints as Syntax (Constraints)
import qualified Syntax.Tree.InstanceDeclaration as Syntax (InstanceDeclaration (..))
import qualified Syntax.Tree.InstanceDeclarations as Syntax (InstanceDeclarations (..))
import qualified Syntax.Tree.TypePattern as Syntax (TypePattern)
import Syntax.Variable (Variable)

resolve ::
  Context scope ->
  Position ->
  Syntax.Constraints Position ->
  Strict.Vector (Syntax.TypePattern Position) ->
  Map Variable Int ->
  Syntax.InstanceDeclarations Position ->
  Instance Identity Identity Normal Resolve scope
resolve
  context
  startPosition
  prerequisites
  parameters
  memberMethods
  declarations
    | parameters <- TypePattern.resolve <$> parameters,
      prerequisites <- Constraints.resolve (Scheme.augmentWith parameters context) prerequisites =
        let members declarations = orderListInt' combine (length memberMethods) members
              where
                combine (member : members) = Definition $ Resolve $ Definition.merge (member :| members)
                combine [] = Generated Inferred
                members = map member $ toList declarations
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
                definition =
                  Identity
                    Header
                      { parameters,
                        prerequisites
                      }
                    ::: Identity
                      ( Identity
                          InstanceDefinition
                            { evidence = Inferred,
                              members = case declarations of
                                Syntax.InstanceDeclarations {declarations} ->
                                  members (toList declarations)
                                Syntax.DerivingInstance ->
                                  Strict.Vector.replicate (length memberMethods) $
                                    Generated Inferred
                            }
                      ),
                prerequisites = Identity Inferred
              }
