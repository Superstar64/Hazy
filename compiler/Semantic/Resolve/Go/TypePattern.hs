module Semantic.Resolve.Go.TypePattern where

import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.TypePattern (TypePattern (..))
import qualified Syntax.Tree.TypePattern as Syntax

resolve :: Syntax.TypePattern position -> TypePattern position Resolve scope
resolve Syntax.TypePattern {position, name} =
  TypePattern
    { position,
      name,
      typex = Inferred
    }
