module Stage4.Tree.Builtin.Tuple where

import Data.Foldable (Foldable (toList))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (Data))
import qualified Stage4.Tree.Data as Data
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

tuple :: Int -> Data scope
tuple n =
  Data
    { parameters = Strict.Vector.replicate n Type.smallType,
      constructors = Strict.Vector.fromList $ toList set,
      selectors = Strict.Vector.empty
    }
  where
    set = map go [minBound .. maxBound]
    go Constructor.Tuple =
      Constructor
        { entries = Strict.Vector.generate n $ Type.Variable . Local.Local
        }
