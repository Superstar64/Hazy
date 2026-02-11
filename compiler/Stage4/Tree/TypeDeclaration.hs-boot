module Stage4.Tree.TypeDeclaration where

import Stage1.Variable (ConstructorIdentifier)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as Solved
import {-# SOURCE #-} Stage4.Tree.Class (Class)
import {-# SOURCE #-} Stage4.Tree.Data (Data)
import {-# SOURCE #-} Stage4.Tree.Type (Type)

data LazyTypeDeclaration scope = !ConstructorIdentifier :^ TypeDeclaration scope

infix 4 :^

data TypeDeclaration scope
  = Data
      { name :: !ConstructorIdentifier,
        datax :: !(Data scope)
      }
  | Class
      { name :: !ConstructorIdentifier,
        classx :: !(Class scope)
      }
  | Synonym
      { name :: !ConstructorIdentifier,
        definition :: !(Type (Local ':+ scope))
      }

assumeData :: TypeDeclaration scope -> Data scope
assumeClass :: TypeDeclaration scope -> Class scope

instance Shift TypeDeclaration

instance Shift.Functor TypeDeclaration

simplify' :: Solved.TypeDeclaration scope -> TypeDeclaration scope
