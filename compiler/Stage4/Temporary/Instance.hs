module Stage4.Temporary.Instance where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Instance as Stage3
import qualified Stage4.Temporary.Definition as Definition
import Stage4.Temporary.Expression (Expression)
import qualified Stage4.Temporary.Expression as Expression
import qualified Stage4.Temporary.Statements as Statements
import Stage4.Tree.Evidence (Evidence)
import qualified Stage4.Tree.Instance as Real

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    memberConstraintCounts :: !(Strict.Vector Int),
    members :: !(Strict.Vector (Expression (Local ':+ Local ':+ scope)))
  }
  deriving (Show)

instance Shift Instance where
  shift = shiftDefault

instance Shift.Functor Instance where
  map = Term.mapDefault

instance Term.Functor Instance where
  map category Instance {evidence, prerequisitesCount, memberConstraintCounts, members} =
    Instance
      { evidence = Term.map (Term.over category) <$> evidence,
        prerequisitesCount,
        memberConstraintCounts,
        members = Term.map (Term.over (Term.over category)) <$> members
      }

simplify :: Stage3.Instance scope -> Instance scope
simplify Stage3.Instance {Stage3.evidence, Stage3.prerequisitesCount, Stage3.memberConstraintCounts, Stage3.members} =
  Instance
    { evidence,
      prerequisitesCount,
      memberConstraintCounts,
      members = go <$> members
    }
  where
    go = \case
      Strict.Just expression -> Definition.desugar $ Definition.simplify expression
      Strict.Nothing -> Expression.Join {Expression.statements = Statements.Bottom}

finish :: Instance scope -> Real.Instance scope
finish Instance {evidence, prerequisitesCount, memberConstraintCounts, members} =
  Real.Instance
    { Real.evidence,
      Real.prerequisitesCount,
      Real.memberConstraintCounts,
      Real.members = Expression.finish <$> members
    }
