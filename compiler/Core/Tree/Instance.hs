module Core.Tree.Instance where

import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (ConstraintCount (..), ConstraintsF (Constraints))
import qualified Core.Tree.Constraints as Constraints
import Core.Tree.Evidence (Evidence)
import Core.Tree.MethodConcrete (MethodConcrete)
import qualified Core.Tree.MethodConcrete as MethodConcrete
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Layout (Normal)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import qualified Semantic.Tree.InstanceDefinition as Semantic (Evidence (..))
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2 (..))

data Instance scope = Instance
  { evidence :: !(Strict.Vector (Evidence (Local ':+ scope))),
    prerequisitesCount :: !ConstraintCount,
    members :: !(Strict.Vector (MethodConcrete scope))
  }
  deriving (Show)

instance Shift0.Functor Instance where
  map = Shift.mapDefault

instance Shift.Functor Instance where
  map = Substitute.mapDefault

instance Substitute.Functor Instance where
  map category Instance {evidence, prerequisitesCount, members} =
    Instance
      { evidence = Substitute.map (Substitute.Over category) <$> evidence,
        prerequisitesCount,
        members = Substitute.map category <$> members
      }

simplify :: Semantic.Instance Identity Identity Normal Check scope -> Instance scope
simplify
  Semantic.Instance
    { definition = _ ::: Identity definition,
      prerequisites = Identity (Solved prerequisites)
    } = case definition of
    Identity InstanceDefinition {evidence = Solved (Semantic.Evidence evidence), members} ->
      Instance
        { evidence,
          prerequisitesCount = case prerequisites of
            Constraints.None -> Null
            Constraints constraints -> ConstraintCount $ length constraints,
          members = MethodConcrete.simplify <$> members
        }
