module Stage4.Tree.Instance where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Layout (Normal)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Stage2.Tree.Instance as Stage3 (Evidence (..), Instance (..))
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Evidence (Evidence)
import Stage4.Tree.MethodConcrete (MethodConcrete)
import qualified Stage4.Tree.MethodConcrete as MethodConcrete

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Evidence (Local ':+ scope))),
    prerequisitesCount :: !Int,
    members :: !(Strict.Vector (MethodConcrete scope))
  }
  deriving (Show)

instance Shift Instance where
  shift = shiftDefault

instance Shift.Functor Instance where
  map = Shift2.mapDefault

instance Shift2.Functor Instance where
  map = Substitute.mapDefault

instance Substitute.Functor Instance where
  map category Instance {evidence, prerequisitesCount, members} =
    Instance
      { evidence = Substitute.map (Substitute.Over category) <$> evidence,
        prerequisitesCount,
        members = Substitute.map category <$> members
      }

simplify :: Stage3.Instance Normal Check scope -> Instance scope
simplify Stage3.Instance {evidence = Solved (Stage3.Evidence evidence), prerequisites, members} =
  Instance
    { evidence,
      prerequisitesCount = length prerequisites,
      members = MethodConcrete.simplify <$> members
    }
