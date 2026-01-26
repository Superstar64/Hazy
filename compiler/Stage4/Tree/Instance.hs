module Stage4.Tree.Instance where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Instance as Stage3 (Instance (..))
import qualified Stage3.Tree.InstanceMethod as Stage3 (InstanceMethod (..))
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Evidence (Evidence)
import Stage4.Tree.Expression (Expression)

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
  map = Shift2.mapDefault

instance Shift2.Functor Instance where
  map = Substitute.mapDefault

instance Substitute.Functor Instance where
  map category Instance {evidence, prerequisitesCount, memberConstraintCounts, members} =
    Instance
      { evidence = Substitute.map (Substitute.Over category) <$> evidence,
        prerequisitesCount,
        memberConstraintCounts,
        members = Substitute.map (Substitute.Over (Substitute.Over category)) <$> members
      }

simplify :: Stage3.Instance scope -> Instance scope
simplify Stage3.Instance {evidence, prerequisitesCount, memberConstraintCounts, members} =
  Instance
    { evidence,
      prerequisitesCount,
      memberConstraintCounts,
      members = Stage3.definition' <$> members
    }
