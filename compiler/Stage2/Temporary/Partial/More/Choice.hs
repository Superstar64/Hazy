module Stage2.Temporary.Partial.More.Choice where

import qualified Stage2.Index.Term as Term (Bound (..))
import Stage2.Tree.Pattern (Pattern)

data Choice scope = Choice
  { temporary :: !Int,
    shareIndex :: Int,
    bound :: Term.Bound,
    patternx :: Pattern scope
  }
