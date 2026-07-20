module Core.Tree.TypeDeclarationExtra where

import {-# SOURCE #-} Core.Tree.ClassExtra (ClassExtra)
import Semantic.Layout (Normal)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import {-# SOURCE #-} qualified Semantic.Tree.TypeDeclarationExtra as Semantic

data TypeDeclarationExtra scope
  = ADT
  | Class !(ClassExtra scope)
  | Synonym
  | GADT

instance Shift0.Functor TypeDeclarationExtra

instance Shift.Functor TypeDeclarationExtra

simplify :: Semantic.TypeDeclarationExtra Normal Check scope -> TypeDeclarationExtra scope
assumeClass :: TypeDeclarationExtra scope -> ClassExtra scope
