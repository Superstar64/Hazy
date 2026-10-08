module Core.Tree.Evidence (Evidence, EvidenceF (..)) where

import qualified Core.Evidence.Functor as Core
import Core.Substitute (Category (..), Evidences (..))
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} qualified Core.Tree.Instanciation as Instanciation
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import Semantic.Shift0 (shift)
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
  map category = Substitute.mapEvidence (Substitute.anyType category)

instance Substitute.EvidenceFunctor EvidenceF where
  mapEvidence
    (Substitute _ _ (Evidences replacements))
    Variable
      { variable = Evidence.Index (Evidence0.Assumed index),
        instanciation = Instanciation.Mono
      } = replacements Vector.! index
  mapEvidence
    (Substitute.Over category)
    Variable
      { variable = Evidence.Index (Evidence0.Shift index),
        instanciation = Instanciation.Mono
      } =
      shift $
        Substitute.mapEvidence
          category
          Variable
            { variable = Evidence.Index index,
              instanciation = Instanciation.Mono
            }
  mapEvidence category evidence = case evidence of
    Variable {variable, instanciation} ->
      Variable
        { variable = Shift.map general variable,
          instanciation = Substitute.mapEvidence (Lift general) instanciation
        }
      where
        general = Substitute.general category
    Super {base, index} ->
      Super
        { base = Substitute.mapEvidence category base,
          index
        }

instance Core.Functor EvidenceF where
  map f g = \case
    Logical logical -> Logical (f logical)
    Variable {variable, instanciation} ->
      Variable
        { variable,
          instanciation = Core.map f g instanciation
        }
    Super {base, index} ->
      Super
        { base = Core.map f g base,
          index
        }
