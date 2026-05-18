module Stage2.Temporary.Partial.More.Choice where

import qualified Stage2.Index.Term as Term (Bound (..), Index)
import Stage2.Stage (Resolve)
import Stage2.Tree.Pattern (Pattern)

data Choice scope = Choice
  { temporary :: !Int,
    index :: Term.Index scope,
    bound :: Term.Bound,
    patternx :: Pattern Resolve scope
  }
