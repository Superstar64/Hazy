module Stage5.Generate.LocalType where

import Data.Map (Map)
import Data.Text (Text)
import qualified Stage2.Index.Type2 as Type2

data LocalType scope = LocalType
  { classInstances :: !(Map (Type2.Index scope) Text),
    dataInstances :: !(Map (Type2.Index scope) Text)
  }
