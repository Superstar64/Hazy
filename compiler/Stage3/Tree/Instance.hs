module Stage3.Tree.Instance where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Data.Vector.Strict (izipWithM)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import qualified Stage2.Shift as Shift
import qualified Stage2.Tree.Instance as Stage2
import Stage3.Check.Context (Context (..))
import Stage3.Check.InstanceAnnotation (InstanceAnnotation (InstanceAnnotation))
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Index.Evidence as Evidence
import qualified Stage3.Index.Evidence0 as Evidence0
import qualified Stage3.Simple.Scheme as Simple.Scheme (augment')
import qualified Stage3.Simple.Type as Simple.Type (lift)
import qualified Stage3.Temporary.Definition as Unsolved.Definition
import Stage3.Tree.InstanceMethod (InstanceMethod (..))
import qualified Stage3.Tree.Scheme as Scheme
import qualified Stage3.Unify as Unify
import Stage4.Substitute (Category (Substitute))
import qualified Stage4.Substitute as Substitute
import {-# SOURCE #-} qualified Stage4.Tree.Builtin as Builtin
import {-# SOURCE #-} qualified Stage4.Tree.Class as Simple.Class
import Stage4.Tree.ClassExtra (ClassExtra (..))
import qualified Stage4.Tree.Constraint as Simple (Constraint (Constraint))
import qualified Stage4.Tree.Constraint as Simple.Constraint
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Evidence as Simple.Evidence
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple.Expression
import qualified Stage4.Tree.Scheme as Simple (Scheme (..))
import qualified Stage4.Tree.Scheme as Simple.Scheme (constraintCount)
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

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    memberConstraintCounts :: !(Strict.Vector Int),
    members :: !(Strict.Vector (InstanceMethod scope))
  }
  deriving (Show)

check ::
  Context s scope ->
  Key scope ->
  InstanceAnnotation scope ->
  Stage2.Instance scope ->
  ST s (Instance scope)
check
  context@Context {typeEnvironment}
  key
  InstanceAnnotation
    { parameters,
      prerequisites
    }
  Stage2.Instance
    { startPosition,
      members
    }
    | index <- index key,
      head <- head key = do
        let prerequisitesCount = length prerequisites
        classx <- do
          let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
          Builtin.index pure get index
        ClassExtra {defaults} <- do
          let get index = Extra.assumeClass <$> TypeBinding.extra (typeEnvironment Table.Type.! index)
          Builtin.index pure get index
        let Simple.Class.Class {constraints, methods} = classx
            base = foldl Simple.Type.Call (shift $ Simple.Type.Constructor head) variables
              where
                variables = [Simple.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
            self
              | null prerequisites = root
              | otherwise =
                  Simple.Evidence.Call
                    { function = root,
                      arguments =
                        Strict.Vector.fromList $ do
                          i <- [0 .. length prerequisites]
                          pure
                            Simple.Evidence.Variable
                              { variable = Evidence.Index (Evidence0.Assumed i)
                              }
                    }
              where
                root =
                  shift
                    Simple.Evidence.Variable
                      { variable = case key of
                          Data {index1, head1} -> Evidence.Data index1 head1
                          Class {index2, head2} -> Evidence.Class index2 head2
                      }
        context <- Scheme.augment startPosition parameters prerequisites context
        let memberConstraintCounts = Simple.Scheme.constraintCount <$> methods
        {-
          instances where the constraints contain the variable in the non head part
          should not kind check, so we should be able to safely ignore them
          For example:
          > instance (F a (a)) => G (H a)
        -}

        evidence <- for constraints $
          \Simple.Constraint {classx, arguments} -> do
            let parameter = foldl Simple.Type.Call base arguments
            evidence <- Unify.constrain context startPosition (shift classx) (Simple.Type.lift parameter)
            Unify.solveEvidence startPosition evidence
        let check _ scheme (Strict.Just member) = do
              context <- Simple.Scheme.augment' startPosition scheme context
              let replacements = Vector.singleton base
                  Simple.Scheme Simple.SchemeOver {result} = scheme
                  category = Substitute.Over $ Substitute Shift.Shift replacements (error "no evidence")
              result <- pure $ Simple.Type.lift $ Substitute.map category result
              definition <- Unsolved.Definition.check context result member
              definition <- Unsolved.Definition.solve definition
              let definition' = Simple.Expression.simplify definition
              pure
                Definition
                  { definition,
                    definition'
                  }
            check index _ Strict.Nothing | defaultx <- defaults Strict.Vector.! index = do
              let typeReplacements = Vector.singleton base
                  evidenceReplacements = Vector.singleton self
                  category = Substitute.Over $ Substitute Shift.Shift typeReplacements evidenceReplacements
                  definition' = Substitute.map category defaultx
              pure Default {definition'}
        members <- izipWithM check methods (fmap shift <$> members)
        pure Instance {evidence, prerequisitesCount, memberConstraintCounts, members}
