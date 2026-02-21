module Stage2.Temporary.Partial.More.Share where

import qualified Stage2.Index.Term as Term (Bound (..))
import Stage2.Tree.Pattern (Pattern)

data Shared scope = Shared
  { shareIndex :: !Int,
    bound :: Term.Bound,
    patternx :: Pattern scope
  }
