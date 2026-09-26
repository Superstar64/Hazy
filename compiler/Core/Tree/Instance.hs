module Core.Tree.Instance where

import qualified Core.Substitute as Substitute
import Core.Tree.Constraints (Constraints)
import Core.Tree.EvidenceSet (EvidenceSet)
import Core.Tree.MethodConcrete (MethodConcrete)
import qualified Core.Tree.MethodConcrete as MethodConcrete
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Layout (Normal)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import Semantic.Tree.InstanceDefinition (InstanceDefinition (..))
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2 (..))

data Instance scope = Instance
  { evidence :: !(EvidenceSet scope),
    prerequisites :: !(Constraints scope),
    members :: !(Strict.Vector (MethodConcrete scope))
  }
  deriving (Show)

instance Shift0.Functor Instance where
  map = Shift.mapDefault

instance Shift.Functor Instance where
  map = Substitute.mapDefault

instance Substitute.Functor Instance where
  map category Instance {evidence, prerequisites, members} =
    Instance
      { evidence = Substitute.map category evidence,
        prerequisites = Substitute.map category prerequisites,
        members = Substitute.map category <$> members
      }

simplify :: Semantic.Instance Identity Identity Normal Check scope -> Instance scope
simplify
  Semantic.Instance
    { definition = _ ::: Identity definition,
      prerequisites = Identity (Solved prerequisites)
    } = case definition of
    Identity InstanceDefinition {evidence = Solved evidence, members} ->
      Instance
        { evidence,
          prerequisites,
          members = MethodConcrete.simplify <$> members
        }
