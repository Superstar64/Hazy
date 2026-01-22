module Stage4.Tree.TypeDeclarationExtra where

import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclarationExtra as Stage3
import {-# SOURCE #-} Stage4.Tree.ClassExtra (ClassExtra)

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT

instance Shift TypeDeclarationExtra

instance Shift.Functor TypeDeclarationExtra

simplify :: Stage3.TypeDeclarationExtra scope -> TypeDeclarationExtra scope
