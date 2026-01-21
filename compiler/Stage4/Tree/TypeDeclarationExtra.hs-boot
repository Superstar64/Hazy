module Stage4.Tree.TypeDeclarationExtra where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclarationExtra as Stage3
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)

data TypeDeclarationExtra scope
  = ADT
  | Class
      { defaults :: !(Strict.Vector (Expression (Local ':+ Local ':+ scope)))
      }
  | Synonym
  | GADT

instance Shift TypeDeclarationExtra

instance Shift.Functor TypeDeclarationExtra

simplify :: Stage3.TypeDeclarationExtra scope -> TypeDeclarationExtra scope
