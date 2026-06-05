module Core.Tree.TypeDeclarationExtra where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.ClassExtra (ClassExtra)
import qualified Core.Tree.ClassExtra as ClassExtra
import Semantic.Layout (Normal)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT
  deriving (Show)

assumeClass :: TypeDeclarationExtra scope -> ClassExtra scope
assumeClass (Class extra) = extra
assumeClass _ = error "bad class extra assumption"

simplify :: Semantic.TypeDeclarationExtra Normal Check scope -> TypeDeclarationExtra scope
simplify = \case
  Semantic.ADT {} -> ADT
  Semantic.Synonym {} -> Synonym
  Semantic.GADT {} -> GADT
  Semantic.Class {methods} -> Class (ClassExtra.simplify methods)

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
