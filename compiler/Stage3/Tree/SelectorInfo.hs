module Stage3.Tree.SelectorInfo where

import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Stage3.Tree.ConstructorInfo (ConstructorInfo)

data SelectorInfo
  = Uniform
      { strict :: !Bool
      }
  | Disjoint
      { select :: !(Strict.Vector Select)
      }
  deriving (Show)

data Select
  = Select
  { selectIndex :: !(Strict.Maybe Int),
    constructorInfo :: !ConstructorInfo
  }
  deriving (Show)
