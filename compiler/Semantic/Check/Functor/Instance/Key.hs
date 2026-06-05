module Semantic.Check.Functor.Instance.Key where

import qualified Semantic.Index.Type2 as Type2

data Key scope
  = Data
      { index :: !Int,
        classKey :: !(Type2.Index scope)
      }
  | Class
      { index :: !Int,
        dataKey :: !(Type2.Index scope)
      }
