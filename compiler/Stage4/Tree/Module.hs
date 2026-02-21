module Stage4.Tree.Module where

import Stage1.Variable (FullQualifiers)
import Stage2.Scope (Global)
import qualified Stage3.Tree.Module as Stage3
import qualified Stage4.Index.Term as Term
import Stage4.Tree.Declarations (Declarations)
import qualified Stage4.Tree.Declarations as Declarations (simplify)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

simplify :: Int -> Stage3.Module -> Module
simplify index Stage3.Module {name, declarations} =
  Module
    { name,
      declarations = Declarations.simplify (Term.Global index) declarations
    }
