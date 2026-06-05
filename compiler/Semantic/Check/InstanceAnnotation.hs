module Semantic.Check.InstanceAnnotation where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constraint as Simple
import qualified Core.Tree.Constraint as Simple.Constraint
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Check.Context (Context)
import qualified Semantic.Check.Temporary.Constraint as Unsolved.Constraint
import qualified Semantic.Check.Temporary.Scheme as Unsolved.Scheme
import qualified Semantic.Check.Temporary.TypePattern as Unsolved
import qualified Semantic.Check.Temporary.TypePattern as Unsolved.TypePattern
import Semantic.Layout (Group)
import Semantic.Stage (Check, Resolve)
import qualified Semantic.Tree.Constraint as Solved (Constraint)
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import qualified Semantic.Tree.TypePattern as Semantic (TypePattern (TypePattern))
import qualified Semantic.Tree.TypePattern as Semantic.TypePattern
import qualified Semantic.Tree.TypePattern as Solved (TypePattern)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data InstanceAnnotation scope = InstanceAnnotation
  { parameters :: !(Strict.Vector (Solved.TypePattern Position Check scope)),
    prerequisites :: !(Strict.Vector (Solved.Constraint Position Check scope)),
    prerequisites' :: !(Strict.Vector (Simple.Constraint scope))
  }

prerequisites'_ = prerequisites'

check ::
  Context s scope ->
  Semantic.Instance Group Resolve scope ->
  ST s (InstanceAnnotation scope)
check context Semantic.Instance {parameters, prerequisites} = do
  let fresh Semantic.TypePattern {name, position} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure
          Unsolved.TypePattern
            { name,
              typex,
              position
            }
  parameters <- traverse fresh parameters
  context <- pure $ Unsolved.Scheme.augment parameters context
  prerequisites <- traverse (Unsolved.Constraint.check context) prerequisites
  parameters <- Unify.runSolve $ traverse Unsolved.TypePattern.solve parameters
  prerequisites <- Unify.runSolve $ traverse (Unsolved.Constraint.solve context) prerequisites
  let prerequisites' = fmap Simple.Constraint.simplify prerequisites
  pure
    InstanceAnnotation
      { parameters,
        prerequisites,
        prerequisites'
      }
