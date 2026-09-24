module Semantic.Check.Go.InstanceDefinition2 where

import Control.Monad.ST (ST)
import qualified Core.Builtin as Builtin
import Core.Substitute (Category (Substitute), logicalType)
import qualified Core.Substitute as Substitute
import qualified Core.Tree.Class as Core.Class
import Core.Tree.ClassExtra (ClassExtra (..))
import qualified Core.Tree.Constraint as Core (ConstraintF (..))
import qualified Core.Tree.Evidence as Core.Evidence
import Core.Tree.EvidenceSet (EvidenceSet (..))
import qualified Core.Tree.Forall as Core (ForallOver (..))
import qualified Core.Tree.Instanciation as Core (InstanciationF (Instanciation, Mono))
import qualified Core.Tree.Instanciation as Core.Instanciation
import Core.Tree.Type ((#))
import qualified Core.Tree.Type as Core (typeWith, universe)
import qualified Core.Tree.Type as Core.Type
import Core.Tree.TypeDeclaration (assumeClass)
import qualified Core.Tree.TypeDeclarationExtra as Extra
import qualified Core.Tree.TypeLambda as Core (TypeLambdaOver (..))
import Data.Functor.Identity (Identity (..))
import Data.Traversable (for)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..))
import Semantic.Check.Go.Definition4 (Solve (..))
import qualified Semantic.Check.Go.Scheme as Scheme
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Scheme as Core.Scheme
import qualified Semantic.Check.Temporary.Constraints as Unsolved.Constraints (check, solve)
import qualified Semantic.Check.Temporary.Definition as Definition
import qualified Semantic.Check.Temporary.Scheme as Unsolved (augment)
import qualified Semantic.Check.Temporary.TypePattern as Unsolved (TypePattern (..), solve)
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Implicit (Implicit (Resolve))
import qualified Semantic.Tree.Combinators.Implicit as Implicit
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.Constraints (Constraints (Constraints, None))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (Header (..), InstanceDefinition2 (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (head)

data Key scope
  = Data
      { index1 :: !(Type2.Index scope),
        head1 :: !(Type.Index scope)
      }
  | Class
      { index2 :: !(Type.Index scope),
        head2 :: !(Type2.Index scope)
      }

keyIndex :: Key scope -> Type2.Index scope
keyIndex = \case
  Data {index1} -> index1
  Class {index2} -> Type2.Index index2

keyHead :: Key scope -> Type2.Index scope
keyHead = \case
  Data {head1} -> Type2.Index head1
  Class {head2} -> head2

check ::
  (Monad solve) =>
  Position ->
  Key scope ->
  Solve solve logical s scope ->
  (declarations (ST s) -> InstanceDefinition2 solve' (ST s) Group Check scope) ->
  (declarations (ST s) -> Context s scope) ->
  InstanceDefinition2 Identity Identity Group Resolve scope ->
  InstanceDefinition2 solve (Topological.Formula declarations s) Group Check scope
check position key Solve {solve} reflection information = \case
  Identity Header {parameters, prerequisites}
    ::: Identity (Identity InstanceDefinition {members}) ->
      Topological.Formula
        { cycle = cyclicalTypeChecking position,
          run = \declarations -> do
            let context = information declarations
                fresh TypePattern {name, position} = do
                  level <- Unify.fresh Core.universe
                  typex <- Unify.fresh (Core.typeWith level)
                  pure
                    Unsolved.TypePattern
                      { name,
                        typex,
                        position
                      }
            parameters <- traverse fresh parameters
            context <- pure $ Unsolved.augment parameters context
            prerequisites <- Unsolved.Constraints.check context prerequisites

            parameters <- Unify.runSolve $ traverse Unsolved.solve parameters
            prerequisites <- Unify.runSolve $ Unsolved.Constraints.solve context prerequisites
            pure Header {parameters, prerequisites}
        }
        ::: Topological.Formula
          { cycle = cyclicalTypeChecking position,
            run = \declarations -> do
              let context@Context {typeEnvironment} = information declarations
                  annotation ::: _ = reflection declarations
                  index = keyIndex key
                  head = keyHead key
              Header {parameters, prerequisites} <- annotation
              Core.Class.Class {constraints, methods} <- do
                let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
                Builtin.index pure get index
              extra <- do
                let get index = do
                      extra <- TypeBinding.extra (typeEnvironment Table.Type.! index)
                      pure (Extra.assumeClass <$> extra)
                Builtin.index (pure . pure) get index
              let base = foldl (#) (shift $ Core.Type.Constructor head) variables
                    where
                      variables = [Core.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
                  self = Core.Evidence.Variable {variable = shift variable, instanciation}
                    where
                      variable = case key of
                        Data {index1, head1} -> Evidence.Data index1 head1
                        Class {index2, head2} -> Evidence.Class index2 head2
                      instanciation = case prerequisites of
                        Semantic.Tree.Constraints.None -> Core.Mono
                        Constraints prerequisites -> Core.Instanciation $ Strict.Vector.fromList $ do
                          i <- [0 .. length prerequisites - 1]
                          pure
                            Core.Evidence.Variable
                              { variable = Evidence.Index (Evidence0.Assumed i),
                                instanciation = Core.Instanciation.Mono
                              }
              context <- Scheme.augment position parameters prerequisites Mask.Runtime context
              {-
                instances where the constraints contain the variable in the non head part
                should not kind check, so we should be able to safely ignore them
                For example:
                > instance (F a (a)) => G (H a)
              -}
              methods <-
                pure $
                  let replacements = Vector.singleton base
                      category = Substitute.Over $ Substitute Shift.Shift replacements (error "no evidence")
                      substitute Core.ForallOver {parameters, constraints, result} =
                        Core.ForallOver
                          { parameters,
                            constraints,
                            result = Substitute.map category result
                          }
                   in substitute <$> methods
              evidence <- for constraints $
                \Core.Constraint {classx, arguments} -> do
                  let parameter = foldl (#) base arguments
                  evidence <- Unify.constrain context position (shift classx) (logicalType parameter)
                  Unify.runSolve $ Unify.solveEvidence position evidence
              let check _ scheme (Definition (Resolve member)) = do
                    let Core.ForallOver {parameters, constraints, result} = scheme
                    result <- pure $ logicalType result
                    context <- Core.Scheme.augmentForall position scheme Mask.Runtime context
                    definition <- Definition.check context result member
                    pure $ do
                      result <- Definition.solve definition
                      pure $
                        Definition $
                          Implicit.Check
                            Core.TypeLambdaOver
                              { parameters,
                                constraints,
                                result
                              }
                  check index scheme Generated {} =
                    pure $ do
                      let Core.ForallOver {parameters, constraints} = scheme
                      defaultx <- do
                        ClassExtra {defaults} <- extra
                        pure $
                          Core.TypeLambdaOver
                            { parameters,
                              constraints,
                              result = defaults Strict.Vector.! index
                            }
                      let typeReplacements = Vector.singleton base
                          evidenceReplacements = Vector.singleton self
                          category = Substitute Shift.Shift typeReplacements evidenceReplacements
                      pure $ Generated $ Solved $ Substitute.map category defaultx
              members <- Strict.Vector.izipWithM check methods (shift <$> members)
              members <- solve $ sequence members
              pure $ do
                members <- members
                pure
                  InstanceDefinition
                    { evidence = Solved $ EvidenceSet $ evidence,
                      members
                    }
          }

solve ::
  InstanceDefinition2 (Unify.Solve s) Identity Group Check scope ->
  Unify.Solve s (InstanceDefinition2 Identity Identity Group Check scope)
solve (annotation ::: Identity definition) = do
  definition <- definition
  pure $ annotation ::: Identity (Identity definition)
