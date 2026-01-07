module Stage3.Simple.TypeDeclaration where

import Stage1.Variable (ConstructorIdentifier)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage3.Simple.Class (Class)
import {-# SOURCE #-} Stage3.Simple.Data (Data)
import {-# SOURCE #-} Stage3.Simple.Type (Type)
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as Solved (TypeDeclaration)

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

simplify :: Solved.TypeDeclaration scope -> TypeDeclaration scope
