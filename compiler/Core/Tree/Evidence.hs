module Core.Tree.Evidence (Evidence, EvidenceF (..)) where

import qualified Core.Evidence.Substitute as Evidence.Substitute
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Type as Type
import qualified Data.Strict.Maybe as Strict
import Data.Void (Void)
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Evidence = EvidenceF Void Void

data EvidenceF logicalEvidence logicalType scope
  = Logical !logicalEvidence
  | Variable
      { variable :: !(Evidence.Index scope),
        instanciation :: !(InstanciationF logicalEvidence logicalType scope)
      }
  | Super
      { base :: !(EvidenceF logicalEvidence logicalType scope),
        index :: !Int
      }
  deriving (Show)

instance
  (Show logicalEvidence, Show logicalType) =>
  Scope.Show (EvidenceF logicalEvidence logicalType)
  where
  showsPrec = showsPrec

instance Shift0.Functor (EvidenceF logicalEvidence logicalType) where
  map = Shift.mapDefault

instance Shift.Functor (EvidenceF logicalEvidence logicalType) where
  map category = \case
    Logical logical -> Logical logical
    Variable {variable, instanciation} ->
      Variable
        { variable = Shift.map category variable,
          instanciation = Shift.map category instanciation
        }
    Super {base, index} ->
      Super
        { base = Shift.map category base,
          index
        }

instance
  (logicalEvidence ~ Void, logicalType ~ Void) =>
  Substitute.Functor (EvidenceF logicalEvidence logicalType)
  where
  map = Evidence.Substitute.mapDefault

instance Evidence.Substitute.Functor EvidenceF where
  map category = \case
    Logical logical -> Logical (Evidence.Substitute.mapEvidence category logical)
    Variable {variable, instanciation}
      | Evidence.Index variable <- variable,
        Strict.Just evidence <-
          Substitute.lookupEvidence (Evidence.Substitute.category category) variable ->
          case instanciation of
            Instanciation.Mono {levity = Type.Constructor Type2.Strict} -> evidence
            _ -> error "instanciation must be strict when substiting evidence"
      | otherwise ->
          Variable
            { variable = Shift.map (Evidence.Substitute.general category) variable,
              instanciation = Evidence.Substitute.map category instanciation
            }
    Super {base, index} ->
      Super
        { base = Evidence.Substitute.map category base,
          index
        }
