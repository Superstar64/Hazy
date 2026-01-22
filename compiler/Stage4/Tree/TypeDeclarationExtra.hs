module Stage4.Tree.TypeDeclarationExtra where

import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.TypeDeclarationExtra as Stage3
import Stage4.Index.Term (mapDefault)
import qualified Stage4.Index.Term as Term
import Stage4.Tree.ClassExtra (ClassExtra)
import qualified Stage4.Tree.ClassExtra as ClassExtra

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT
  deriving (Show)

simplify :: Stage3.TypeDeclarationExtra scope -> TypeDeclarationExtra scope
simplify = \case
  Stage3.ADT -> ADT
  Stage3.Synonym -> Synonym
  Stage3.GADT -> GADT
  Stage3.Class {defaults} -> Class (ClassExtra.simplify defaults)

instance Shift TypeDeclarationExtra where
  shift = shiftDefault

instance Shift.Functor TypeDeclarationExtra where
  map = mapDefault

instance Term.Functor TypeDeclarationExtra where
  map category = \case
    ADT -> ADT
    GADT -> GADT
    Synonym -> Synonym
    Class extra -> Class (Term.map category extra)
