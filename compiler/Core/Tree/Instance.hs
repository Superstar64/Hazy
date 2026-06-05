module Core.Tree.Instance where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (Evidence)
import Core.Tree.MethodConcrete (MethodConcrete)
import qualified Core.Tree.MethodConcrete as MethodConcrete
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift (shift), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Instance as Semantic (Evidence (..), Instance (..))

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

simplify :: Semantic.Instance Normal Check scope -> Instance scope
simplify Semantic.Instance {evidence = Solved (Semantic.Evidence evidence), prerequisites, members} =
  Instance
    { evidence,
      prerequisitesCount = length prerequisites,
      members = MethodConcrete.simplify <$> members
    }
