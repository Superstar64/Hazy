module Stage4.Tree.Builtin.List where

import Data.Foldable (Foldable (toList))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage1.Tree.Brand as Brand
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (Data))
import qualified Stage4.Tree.Data as Data
import Stage4.Tree.Entry (Entry (..))
import qualified Stage4.Tree.Type as Type (Type (..), smallType)

list :: Data scope
list =
  Data
    { parameters = Strict.Vector.singleton Type.smallType,
      constructors = Strict.Vector.fromList $ toList set,
      selectors = Strict.Vector.empty,
      brand = Brand.Boxed
    }
  where
    set = map go [minBound .. maxBound]
      where
        go Constructor.Nil = Constructor {entries = Strict.Vector.empty}
        go Constructor.Cons = Constructor {entries = Strict.Vector.fromList [head, tail]}
          where
            head =
              Entry
                { entry = Type.Variable $ Local.Local 0,
                  strict = Type.Constructor Type2.Lazy
                }
            tail =
              Entry
                { entry = Type.Constructor Type2.List `Type.Call` Type.Variable (Local.Local 0),
                  strict = Type.Constructor Type2.Lazy
                }
