module Semantic.Tree.InstanceDefinition where

import qualified Core.Tree.EvidenceSet as Core
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))

data InstanceDefinition origin layout stage scope = InstanceDefinition
  { evidence :: !(Inferred Core.EvidenceSet stage scope),
    members :: !(Strict.Vector (MethodConcrete origin layout stage scope))
  }
  deriving (Show)

instance Shift0.Functor (InstanceDefinition origin layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (InstanceDefinition origin layout stage) where
  map category InstanceDefinition {evidence, members} =
    InstanceDefinition
      { evidence = Shift.map category evidence,
        members = Shift.map category <$> members
      }

instance Connect (InstanceDefinition origin) where
  connect InstanceDefinition {evidence, members} =
    InstanceDefinition
      { evidence,
        members = connect <$> members
      }
  seperate InstanceDefinition {evidence, members} =
    InstanceDefinition
      { evidence,
        members = seperate <$> members
      }
