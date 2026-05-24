module Stage4.Tree.TypeDeclarationExtra where

import Stage2.Layout (Normal)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import {-# SOURCE #-} qualified Stage2.Tree.TypeDeclarationExtra as Stage3
import {-# SOURCE #-} Stage4.Tree.ClassExtra (ClassExtra)

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT

instance Shift TypeDeclarationExtra

instance Shift.Functor TypeDeclarationExtra

simplify :: Stage3.TypeDeclarationExtra Normal Check scope -> TypeDeclarationExtra scope
assumeClass :: TypeDeclarationExtra scope -> ClassExtra scope
