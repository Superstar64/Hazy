module Stage5.Generate.GlobalType where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Scope as Scope
import Stage5.Generate.Global (Global)

data GlobalType = GlobalType
  { classInstances :: !(Map (Type2.Index Scope.Global) Global),
    dataInstances :: !(Map (Type2.Index Scope.Global) Global)
  }

indexClass :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {classInstances} `indexClass` index = classInstances Map.! index

indexData :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {dataInstances} `indexData` index = dataInstances Map.! index
