module Stage4.Tree.TypeDeclarationExtra where

import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import qualified Stage2.Tree.TypeDeclarationExtra as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.ClassExtra (ClassExtra)
import qualified Stage4.Tree.ClassExtra as ClassExtra

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT
  deriving (Show)

assumeClass :: TypeDeclarationExtra scope -> ClassExtra scope
assumeClass (Class extra) = extra
assumeClass _ = error "bad class extra assumption"

simplify :: Stage3.TypeDeclarationExtra Normal Check scope -> TypeDeclarationExtra scope
simplify = \case
  Stage3.ADT {} -> ADT
  Stage3.Synonym {} -> Synonym
  Stage3.GADT {} -> GADT
  Stage3.Class {methods} -> Class (ClassExtra.simplify methods)

instance Shift TypeDeclarationExtra where
  shift = shiftDefault

instance Shift.Functor TypeDeclarationExtra where
  map = Shift2.mapDefault

instance Shift2.Functor TypeDeclarationExtra where
  map = Substitute.mapDefault

instance Substitute.Functor TypeDeclarationExtra where
  map category = \case
    ADT -> ADT
    GADT -> GADT
    Synonym -> Synonym
    Class extra -> Class (Substitute.map category extra)
