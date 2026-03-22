module Stage4.Tree.Builtin.Ordering where

import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Tree.Brand (Brand (..))
import qualified Stage2.Index.Constructor as Constructor
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (Data))
import qualified Stage4.Tree.Data as Data

ordering :: Data scope
ordering =
  Data
    { parameters = Strict.Vector.empty,
      constructors = Strict.Vector.fromList set,
      selectors = Strict.Vector.empty,
      brand = Boxed
    }
  where
    set = map go [minBound .. maxBound]
      where
        go Constructor.LT = Constructor {entries = Strict.Vector.empty}
        go Constructor.EQ = Constructor {entries = Strict.Vector.empty}
        go Constructor.GT = Constructor {entries = Strict.Vector.empty}
