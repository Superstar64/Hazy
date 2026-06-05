module Generate.GlobalType where

import Data.Map (Map)
import qualified Data.Map as Map
import Generate.Global (Global)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Scope as Scope

data GlobalType = GlobalType
  { classInstances :: !(Map (Type2.Index Scope.Global) Global),
    dataInstances :: !(Map (Type2.Index Scope.Global) Global)
  }

indexClass :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {classInstances} `indexClass` index = classInstances Map.! index

indexData :: GlobalType -> Type2.Index Scope.Global -> Global
GlobalType {dataInstances} `indexData` index = dataInstances Map.! index
