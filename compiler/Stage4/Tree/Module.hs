module Stage4.Tree.Module where

import Stage1.Variable (FullQualifiers)
import Stage2.Scope (Global)
import qualified Stage3.Tree.Module as Stage3
import qualified Stage4.Temporary.Declarations as Declarations (finish, simplify)
import Stage4.Tree.Declarations (Declarations)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

simplify :: Stage3.Module -> Module
simplify Stage3.Module {Stage3.name, Stage3.declarations} =
  Module
    { name,
      declarations = Declarations.finish (Declarations.simplify declarations)
    }
