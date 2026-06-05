module Core.Tree.Builtin.Ordering where

import Core.Tree.Constructor (Constructor (..))
import Core.Tree.Data (Data (Data))
import qualified Core.Tree.Data as Data
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import Syntax.Tree.Brand (Brand (..))

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
