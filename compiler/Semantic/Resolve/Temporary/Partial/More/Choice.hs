module Semantic.Resolve.Temporary.Partial.More.Choice where

import qualified Semantic.Index.Term as Term (Bound (..), Index)
import Semantic.Stage (Resolve)
import Semantic.Tree.Pattern (Pattern)

data Choice scope = Choice
  { temporary :: !Int,
    index :: Term.Index scope,
    bound :: Term.Bound,
    patternx :: Pattern Resolve scope
  }
