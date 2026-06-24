module Semantic.Check.Temporary.Instance where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Builtin as Builtin
import Core.Substitute (Category (Substitute))
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} qualified Core.Tree.Class as Simple.Class
import Core.Tree.ClassExtra (ClassExtra (..))
import qualified Core.Tree.Constraint as Simple (Constraint (Constraint))
import qualified Core.Tree.Constraint as Simple.Constraint (Constraint (..))
import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Evidence as Simple.Evidence
import qualified Core.Tree.Instanciation as Simple (Instanciation (..))
import qualified Core.Tree.Instanciation as Simple.Instanciation
import qualified Core.Tree.Scheme as Simple (Scheme (..))
import qualified Core.Tree.SchemeOver as Simple (SchemeOver (..))
import qualified Core.Tree.Type as Simple.Type (Type (..))
import Core.Tree.TypeDeclaration (assumeClass)
import qualified Core.Tree.TypeDeclarationExtra as Extra
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Data.Vector.Strict (izipWithM)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context (..))
import qualified Semantic.Check.Go.Scheme as Scheme
import Semantic.Check.InstanceAnnotation (InstanceAnnotation (InstanceAnnotation))
import qualified Semantic.Check.InstanceAnnotation as InstanceAnnotation
import qualified Semantic.Check.Mask as Mask
import qualified Semantic.Check.Simple.Constraints as Simple.Constraints
import qualified Semantic.Check.Simple.Scheme as Simple.Scheme (augment')
import qualified Semantic.Check.Simple.Type as Simple.Type (lift)
import qualified Semantic.Check.Temporary.Definition as Definition
import Semantic.Check.Temporary.MethodConcrete (MethodConcrete (..))
import qualified Semantic.Check.Temporary.MethodConcrete as MethodConcrete
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Table.Type as Table.Type
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Combinators.Implicit as Semantic (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Constraints as Solved (Constraints (Constraints))
import qualified Semantic.Tree.Constraints as Solved.Constraints
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import qualified Semantic.Tree.Instance as Solved (Evidence (..), Instance (..))
import qualified Semantic.Tree.MethodConcrete as Semantic (MethodConcrete (..))
import qualified Semantic.Tree.TypePattern as Solved (TypePattern)
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

index :: Key scope -> Type2.Index scope
index = \case
  Data {index1} -> index1
  Class {index2} -> Type2.Index index2

head :: Key scope -> Type2.Index scope
head = \case
  Data {head1} -> Type2.Index head1
  Class {head2} -> head2

data Instance s scope = Instance
  { startPosition :: !Position,
    parameters :: !(Strict.Vector (Solved.TypePattern Position Check scope)),
    prerequisites :: !(Solved.Constraints Position Check scope),
    evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    members :: !(Strict.Vector (MethodConcrete s scope))
  }

check ::
  Context s scope ->
  Key scope ->
  InstanceAnnotation scope ->
  Semantic.Instance Group Resolve scope ->
  ST s (Instance s scope)
check
  context@Context {typeEnvironment}
  key
  InstanceAnnotation
    { parameters,
      prerequisites
    }
  Semantic.Instance
    { startPosition,
      members
    }
    | index <- index key,
      head <- head key = do
        classx <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
          Builtin.index pure get index
        extra <- do
          let get index = do
                extra <- TypeBinding.extra (typeEnvironment Table.Type.! index)
                pure (Extra.assumeClass <$> extra)
          Builtin.index (pure . pure) get index
        let Simple.Class.Class {constraints, methods} = classx
            base = foldl Simple.Type.Call (shift $ Simple.Type.Constructor head) variables
              where
                variables = [Simple.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
            self = Simple.Evidence.Variable {variable = shift variable, instanciation}
              where
                variable = case key of
                  Data {index1, head1} -> Evidence.Data index1 head1
                  Class {index2, head2} -> Evidence.Class index2 head2
                instanciation = case prerequisites of
                  Solved.Constraints.None -> Simple.Mono
                  Solved.Constraints prerequisites -> Simple.Instanciation $ Strict.Vector.fromList $ do
                    i <- [0 .. length prerequisites - 1]
                    pure
                      Simple.Evidence.Variable
                        { variable = Evidence.Index (Evidence0.Assumed i),
                          instanciation = Simple.Instanciation.Mono
                        }
        context <- Scheme.augment startPosition parameters prerequisites Mask.Runtime context
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
                substitute (Simple.Scheme Simple.SchemeOver {parameters, constraints, result}) =
                  Simple.Scheme
                    Simple.SchemeOver
                      { parameters,
                        constraints,
                        result = Substitute.map category result
                      }
             in substitute <$> methods

        evidence <- for constraints $
          \Simple.Constraint {classx, arguments} -> do
            let parameter = foldl Simple.Type.Call base arguments
            evidence <- Unify.constrain context startPosition (shift classx) (Simple.Type.lift parameter)
            Unify.runSolve $ Unify.solveEvidence startPosition evidence
        let check _ scheme Semantic.Definition {definition = Semantic.Resolve member} = do
              let Simple.Scheme Simple.SchemeOver {parameters, constraints, result} = scheme
              result <- pure $ Simple.Type.lift result
              context <- Simple.Scheme.augment' startPosition scheme Mask.Runtime context
              definition <- Definition.check context result member
              pure
                Definition
                  { position = startPosition,
                    definition =
                      Unify.schemeOver
                        (Simple.Type.lift <$> parameters)
                        (Simple.Constraints.lift constraints)
                        definition
                  }
            check index scheme Semantic.Default {} = do
              let Simple.Scheme Simple.SchemeOver {parameters, constraints} = scheme
                  defaultx = do
                    ClassExtra {defaults} <- extra
                    pure $
                      Simple.SchemeOver
                        { parameters,
                          constraints,
                          result = defaults Strict.Vector.! index
                        }
              pure Default {self, base, defaultx}
        members <- izipWithM check methods (shift <$> members)
        pure Instance {startPosition, parameters, prerequisites, evidence, members}

solve :: Instance s scope -> Unify.Solve s (Solved.Instance Group Check scope)
solve Instance {startPosition, parameters, prerequisites, evidence, members} = do
  members <- traverse MethodConcrete.solve members
  pure
    Solved.Instance
      { startPosition,
        parameters,
        prerequisites,
        evidence = Solved $ Solved.Evidence evidence,
        members
      }
