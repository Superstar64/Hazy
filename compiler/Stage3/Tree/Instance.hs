module Stage3.Tree.Instance where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe)
import Data.Traversable (for)
import Data.Vector.Strict (zipWithM)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Instance as Stage2
import Stage3.Check.Context (Context (..))
import Stage3.Check.InstanceAnnotation (InstanceAnnotation (InstanceAnnotation))
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Simple.Builtin as Builtin
import qualified Stage3.Simple.Class as Simple.Class
import qualified Stage3.Simple.Constraint as Simple (Constraint (Constraint))
import qualified Stage3.Simple.Constraint as Simple.Constraint
import qualified Stage3.Simple.Evidence as Simple (Evidence)
import qualified Stage3.Simple.Scheme as Simple (Scheme (..))
import qualified Stage3.Simple.Scheme as Simple.Scheme
import qualified Stage3.Simple.SchemeOver as Simple (SchemeOver (..))
import qualified Stage3.Simple.Type as Simple.Type
import {-# SOURCE #-} Stage3.Simple.TypeDeclaration (assumeClass)
import qualified Stage3.Temporary.Definition as Unsolved.Definition
import Stage3.Tree.Definition (Definition)
import qualified Stage3.Tree.Scheme as Scheme
import qualified Stage3.Unify as Unify

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Simple.Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    memberConstraintCounts :: !(Strict.Vector Int),
    members :: !(Strict.Vector (Strict.Maybe (Definition (Local ':+ Local ':+ scope))))
  }
  deriving (Show)

check ::
  Context s scope ->
  Type2.Index scope ->
  Type2.Index scope ->
  InstanceAnnotation scope ->
  Stage2.Instance scope ->
  ST s (Instance scope)
check
  context@Context {typeEnvironment}
  classx
  head
  InstanceAnnotation
    { InstanceAnnotation.parameters,
      InstanceAnnotation.prerequisites
    }
  Stage2.Instance
    { Stage2.startPosition,
      Stage2.members
    } = do
    let prerequisitesCount = length prerequisites
    classx <- do
      let get index = assumeClass <$> TypeBinding.content (typeEnvironment Table.Type.! index)
      Builtin.index pure get classx
    let Simple.Class.Class {Simple.Class.constraints, Simple.Class.methods} = classx
        base = foldl Simple.Type.Call (shift $ Simple.Type.Constructor head) variables
          where
            variables = [Simple.Type.Variable $ Local.Local i | i <- [0 .. length parameters - 1]]
    context <- Scheme.augment startPosition parameters prerequisites context
    let memberConstraintCounts = Simple.Scheme.constraintCount <$> methods
    {-
      instances where the constraints contain the variable in the non head part
      should not kind check, so we should be able to safely ignore them
      For example:
      > instance (F a (a)) => G (H a)
    -}

    evidence <- for constraints $
      \Simple.Constraint {Simple.Constraint.classx, Simple.Constraint.arguments} -> do
        let parameter = foldl Simple.Type.Call base arguments
        evidence <- Unify.constrain context startPosition (shift classx) (Simple.Type.lift parameter)
        Unify.solveEvidence evidence
    let check scheme member = for member $ \member -> do
          context <- Simple.Scheme.augment' startPosition scheme context
          let Simple.Scheme Simple.SchemeOver {Simple.result} = scheme
              replace = \case
                Local.Shift (Local.Local index)
                  | 0 <- index -> shift base
                  | otherwise -> error "bad replace"
                index -> Simple.Type.Variable index
          result <- pure $ Simple.Type.lift $ Simple.Type.substitute replace Simple.Type.Constructor result
          Unsolved.Definition.check context result member
    members <- zipWithM check methods (fmap shift <$> members)
    members <- traverse (traverse Unsolved.Definition.solve) members
    pure Instance {evidence, prerequisitesCount, memberConstraintCounts, members}
