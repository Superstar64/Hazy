module Stage4.Tree.Builtin.Bool where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage2.Index.Constructor as Constructor
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (Data))
import qualified Stage4.Tree.Data as Data

bool :: Data scope
bool =
  Data
    { parameters = Strict.Vector.empty,
      constructors = Strict.Vector.fromList set,
      selectors = Strict.Vector.empty
    }
  where
    set = map go [minBound .. maxBound]
      where
        go Constructor.False = Constructor {entries = Strict.Vector.empty}
        go Constructor.True = Constructor {entries = Strict.Vector.empty}
