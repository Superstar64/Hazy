module Semantic.Tree.InstanceDefinition where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Connect (Connect (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.MethodConcrete (MethodConcrete (..))

data InstanceDefinition origin layout stage scope = InstanceDefinition
  { evidence :: !(Inferred Evidence stage scope),
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

newtype Evidence scope = Evidence (Strict.Vector (Simple.Evidence (Local ':+ scope)))
  deriving (Show)

instance Scope.Show Evidence where
  showsPrec = showsPrec

instance Shift0.Functor Evidence where
  map = Shift.mapDefault

instance Shift.Functor Evidence where
  map category (Evidence evidence) = Evidence (Shift.map (Shift.Over category) <$> evidence)
