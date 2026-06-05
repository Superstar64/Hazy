module Core.Tree.TypeDeclarationExtra where

import {-# SOURCE #-} Core.Tree.ClassExtra (ClassExtra)
import Semantic.Layout (Normal)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.TypeDeclarationExtra as Semantic

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT

instance Shift TypeDeclarationExtra

instance Shift.Functor TypeDeclarationExtra

simplify :: Semantic.TypeDeclarationExtra Normal Check scope -> TypeDeclarationExtra scope
assumeClass :: TypeDeclarationExtra scope -> ClassExtra scope
