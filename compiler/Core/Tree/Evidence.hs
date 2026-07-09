module Core.Tree.Evidence (Evidence, EvidenceF (..)) where

import qualified Core.Shift as Shift2
import Core.Substitute (Category (Substitute))
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} qualified Core.Tree.Instanciation as Instanciation
import qualified Data.Vector as Vector
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Evidence0 as Evidence0
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Equal (..), IsVacuous (isVacuous), Vacuous)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift

type Evidence = EvidenceF Vacuous

data EvidenceF logical scope
  = Logical !(logical scope)
  | Variable
      { variable :: !(Evidence.Index scope),
        instanciation :: !(InstanciationF logical scope)
      }
  | Super
      { base :: !(EvidenceF logical scope),
        index :: !Int
      }

instance (Scope.Show logical) => Show (EvidenceF logical scope) where
  showsPrec d = \case
    Logical logical -> showParen (d > 10) $ showString "Logical " . Scope.showsPrec 11 logical
    Variable {variable, instanciation} ->
      showString "Variable { variable = "
        . showsPrec 11 variable
        . showString ", instanciation = "
        . showsPrec 11 instanciation
        . showString " }"
    Super {base, index} ->
      showString "Super { base = "
        . showsPrec 11 base
        . showString ", index = "
        . showsPrec 11 index
        . showString " }"

instance (Scope.Show logical) => Scope.Show (EvidenceF logical) where
  showsPrec = showsPrec

instance (Shift.Functor logical) => Shift (EvidenceF logical) where
  shift = shiftDefault

instance (Shift.Functor logical) => Shift.Functor (EvidenceF logical) where
  map category = \case
    Logical logical -> Logical (Shift.map category logical)
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

instance (IsVacuous logical, Shift.Functor logical) => Shift2.Functor (EvidenceF logical) where
  map = Substitute.mapDefault

instance (IsVacuous logical, Shift.Functor logical) => Substitute.Functor (EvidenceF logical) where
  map
    (Substitute _ _ replacements)
    Variable
      { variable = Evidence.Index (Evidence0.Assumed index),
        instanciation = Instanciation.Mono
      } | Refl <- isVacuous :: Scope.Equal Vacuous logical = replacements Vector.! index
  map
    (Substitute.Over category)
    Variable
      { variable = Evidence.Index (Evidence0.Shift index),
        instanciation = Instanciation.Mono
      } =
      shift $
        Substitute.map
          category
          Variable
            { variable = Evidence.Index index,
              instanciation = Instanciation.Mono
            }
  map category evidence | Refl <- isVacuous :: Scope.Equal Vacuous logical = case evidence of
    Variable {variable, instanciation} ->
      Variable
        { variable =
            let map1 :: Category scope1 scope2 -> Type.Index scope1 -> Type.Index scope2
                map1 (Substitute.Lift category) index = Shift2.map category index
                map1 (Substitute category _ _) index = Shift.map category $ Type.unlocal index
                map1 (Substitute.Over category) (Type.Shift index) = Type.Shift $ map1 category index
                map1 Substitute.Over {} (Type.Declaration index) = Type.Declaration index
                map1 Substitute.Over {} (Type.Group index) = Type.Group index
                map2 :: Category scope1 scope2 -> Type2.Index scope1 -> Type2.Index scope2
                map2 = Type2.map . map1
                map3 :: Category scope1 scope2 -> Evidence0.Index scope1 -> Evidence0.Index scope2
                map3 (Substitute.Lift category) index = Shift2.map category index
                map3 Substitute {} Evidence0.Assumed {} =
                  error "can't substitute evidence into instanciated evidence variable"
                map3 Substitute.Over {} (Evidence0.Assumed index) = Evidence0.Assumed index
                map3 (Substitute.Over category) (Evidence0.Shift index) =
                  Evidence0.Shift $ map3 category index
                map3 (Substitute category _ _) (Evidence0.Shift index) = Shift.map category index
             in case variable of
                  Evidence.Builtin builtin -> Evidence.Builtin builtin
                  Evidence.Class index1 index2 -> Evidence.Class (map1 category index1) (map2 category index2)
                  Evidence.Data index1 index2 -> Evidence.Data (map2 category index1) (map1 category index2)
                  Evidence.Index index -> Evidence.Index $ map3 category index,
          instanciation = Substitute.map category instanciation
        }
    Super {base, index} ->
      Super
        { base = Substitute.map category base,
          index
        }
