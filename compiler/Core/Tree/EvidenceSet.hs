module Core.Tree.EvidenceSet where

import qualified Core.Substitute as Substitute
import Core.Tree.Evidence (Evidence)
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

newtype EvidenceSet scope = EvidenceSet (Strict.Vector (Evidence (Local ':+ scope)))
  deriving (Show)

instance Scope.Show EvidenceSet where
  showsPrec = showsPrec

instance Shift0.Functor EvidenceSet where
  map = Shift.mapDefault

instance Shift.Functor EvidenceSet where
  map category (EvidenceSet evidence) = EvidenceSet (Shift.map (Shift.Over category) <$> evidence)

instance Substitute.Functor EvidenceSet where
  map category (EvidenceSet evidence) = EvidenceSet $ Substitute.map (Substitute.Over category) <$> evidence
