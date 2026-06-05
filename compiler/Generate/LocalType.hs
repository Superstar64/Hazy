module Generate.LocalType where

import Data.Map (Map)
import Data.Text (Text)
import qualified Semantic.Index.Type2 as Type2

data LocalType scope = LocalType
  { classInstances :: !(Map (Type2.Index scope) Text),
    dataInstances :: !(Map (Type2.Index scope) Text)
  }
