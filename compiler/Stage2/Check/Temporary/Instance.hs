module Stage2.Check.Temporary.Instance where

import Control.Monad.ST (ST)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Data.Vector.Strict (izipWithM)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import Stage2.Check.Context (Context (..))
import qualified Stage2.Check.Go.Scheme as Scheme
import Stage2.Check.InstanceAnnotation (InstanceAnnotation (InstanceAnnotation))
import qualified Stage2.Check.InstanceAnnotation as InstanceAnnotation
import qualified Stage2.Check.Mask as Mask
import qualified Stage2.Check.Simple.Constraint as Simple.Constraint (lift)
import qualified Stage2.Check.Simple.Scheme as Simple.Scheme (augment')
import qualified Stage2.Check.Simple.Type as Simple.Type (lift)
import qualified Stage2.Check.Temporary.Definition as Definition
import Stage2.Check.Temporary.MethodConcrete (MethodConcrete (..))
import qualified Stage2.Check.Temporary.MethodConcrete as MethodConcrete
import qualified Stage2.Check.TypeBinding as TypeBinding
import qualified Stage2.Index.Evidence as Evidence
import qualified Stage2.Index.Evidence0 as Evidence0
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Implicit as Stage2 (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Constraint as Solved (Constraint)
import qualified Stage2.Tree.Instance as Solved (Evidence (..), Instance (..))
import qualified Stage2.Tree.Instance as Stage2 (Instance (..))
import qualified Stage2.Tree.MethodConcrete as Stage2 (MethodConcrete (..))
import qualified Stage2.Tree.TypePattern as Solved (TypePattern)
import qualified Stage2.Unify as Unify
import Stage4.Substitute (Category (Substitute))
import qualified Stage4.Substitute as Substitute
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} qualified Stage4.Tree.Class as Simple.Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import qualified Stage4.Tree.Constraint as Simple (Constraint (Constraint))
import qualified Stage4.Tree.Constraint as Simple.Constraint (Constraint (..))
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Evidence as Simple.Evidence
import qualified Stage4.Tree.Instanciation as Simple (Instanciation (Instanciation))
import qualified Stage4.Tree.Instanciation as Simple.Instanciation
import qualified Stage4.Tree.Scheme as Simple (Scheme (..))
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver (..))
import qualified Stage4.Tree.Type as Simple.Type (Type (..))
import Stage4.Tree.TypeDeclaration (assumeClass)
import qualified Stage4.Tree.TypeDeclarationExtra as Extra
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
  { startPosition, classPosition :: !Position,
    parameters :: !(Strict.Vector (Solved.TypePattern Position Check scope)),
    prerequisites :: !(Strict.Vector (Solved.Constraint Position Check scope)),
    evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    members :: !(Strict.Vector (MethodConcrete s scope))
  }

check ::
  Context s scope ->
  Key scope ->
  InstanceAnnotation scope ->
  Stage2.Instance Group Resolve scope ->
  ST s (Instance s scope)
check
  context@Context {typeEnvironment}
  key
  InstanceAnnotation
    { parameters,
      prerequisites
    }
  Stage2.Instance
    { startPosition,
      classPosition,
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
                instanciation = Simple.Instanciation $ Strict.Vector.fromList $ do
                  i <- [0 .. length prerequisites - 1]
                  pure
                    Simple.Evidence.Variable
                      { variable = Evidence.Index (Evidence0.Assumed i),
                        instanciation = Simple.Instanciation.empty
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
        let check _ scheme Stage2.Definition {definition = Stage2.Resolve member} = do
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
                        (Simple.Constraint.lift <$> constraints)
                        definition
                  }
            check index scheme Stage2.Default {} = do
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
        pure Instance {startPosition, classPosition, parameters, prerequisites, evidence, members}

solve :: Instance s scope -> Unify.Solve s (Solved.Instance Group Check scope)
solve Instance {startPosition, classPosition, parameters, prerequisites, evidence, members} = do
  members <- traverse MethodConcrete.solve members
  pure
    Solved.Instance
      { startPosition,
        classPosition,
        parameters,
        prerequisites,
        evidence = Solved $ Solved.Evidence evidence,
        members
      }
