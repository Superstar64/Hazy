module Stage3.Functor.Instance.Key where

import qualified Stage2.Index.Type2 as Type2

data Key scope
  = Data
      { index :: !Int,
        classKey :: !(Type2.Index scope)
      }
  | Class
      { index :: !Int,
        dataKey :: !(Type2.Index scope)
      }
