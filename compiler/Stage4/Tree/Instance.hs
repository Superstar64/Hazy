module Stage4.Tree.Instance where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Instance as Stage3
import qualified Stage4.Index.Term as Term
import Stage4.Tree.Evidence (Evidence)
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import qualified Stage4.Tree.Statements as Statements

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
      { evidence = Term.map (Term.Over category) <$> evidence,
        prerequisitesCount,
        memberConstraintCounts,
        members = Term.map (Term.Over (Term.Over category)) <$> members
      }

simplify :: Stage3.Instance scope -> Instance scope
simplify Stage3.Instance {evidence, prerequisitesCount, memberConstraintCounts, members} =
  Instance
    { evidence,
      prerequisitesCount,
      memberConstraintCounts,
      members = go <$> members
    }
  where
    go = \case
      Strict.Just expression -> Expression.simplify expression
      Strict.Nothing -> Expression.Join {statements = Statements.Bottom}
