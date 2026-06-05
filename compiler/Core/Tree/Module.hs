module Core.Tree.Module where

import Core.Tree.Declarations (Declarations)
import qualified Core.Tree.Declarations as Declarations (simplify)
import qualified Semantic.Check.Go.Module as Semantic
import Semantic.Layout (Normal)
import Semantic.Scope (Global)
import Semantic.Stage (Check)
import Syntax.Variable (FullQualifiers)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

simplify :: Semantic.Module Normal Check -> Module
simplify Semantic.Module {name, declarations} =
  Module
    { name,
      declarations = Declarations.simplify declarations
    }
