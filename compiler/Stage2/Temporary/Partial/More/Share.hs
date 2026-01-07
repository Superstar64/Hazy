module Stage2.Temporary.Partial.More.Share where

import qualified Stage2.Index.Term as Term (Bound (..))

data Shared = Shared
  { shareIndex :: !Int,
    bound :: Term.Bound
  }
