module Stage3.Check.InstanceAnnotation where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Tree.Instance as Stage2 (Instance (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as Stage2.TypePattern
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Constraint as Simple
import qualified Stage3.Simple.Constraint as Simple.Constraint
import qualified Stage3.Synonym as Synonym
import qualified Stage3.Temporary.Constraint as Unsolved.Constraint
import qualified Stage3.Temporary.Scheme as Unsolved.Scheme
import qualified Stage3.Temporary.TypePattern as Unsolved
import qualified Stage3.Temporary.TypePattern as Unsolved.TypePattern
import qualified Stage3.Tree.Constraint as Solved (Constraint)
import qualified Stage3.Tree.TypePattern as Solved (TypePattern)
import qualified Stage3.Unify as Unify

data InstanceAnnotation scope = InstanceAnnotation
  { parameters :: !(Strict.Vector (Solved.TypePattern scope)),
    prerequisites :: !(Strict.Vector (Solved.Constraint scope)),
    prerequisites' :: !(Strict.Vector (Simple.Constraint scope))
  }

prerequisites'_ = prerequisites'

check :: Context s scope -> Stage2.Instance scope -> ST s (InstanceAnnotation scope)
check context Stage2.Instance {Stage2.parameters, Stage2.prerequisites} = do
  let fresh Stage2.TypePattern {Stage2.TypePattern.name, Stage2.TypePattern.position} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure
          Unsolved.TypePattern
            { Unsolved.TypePattern.name,
              Unsolved.TypePattern.typex,
              Unsolved.TypePattern.position
            }
  parameters <- traverse fresh parameters
  context <- pure $ Unsolved.Scheme.augment parameters context
  let simplify = Synonym.fromProper context
  prerequisites <- traverse (Unsolved.Constraint.check context) $ prerequisites
  parameters <- traverse Unsolved.TypePattern.solve parameters
  prerequisites <- traverse (Unsolved.Constraint.solve simplify) prerequisites
  let prerequisites' = fmap Simple.Constraint.simplify prerequisites
  pure
    InstanceAnnotation
      { parameters,
        prerequisites,
        prerequisites'
      }
