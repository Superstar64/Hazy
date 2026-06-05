module Core.Tree.Builtin.List where

import Core.Tree.Constructor (Constructor (..))
import Core.Tree.Data (Data (Data))
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (Entry (..))
import qualified Core.Tree.Type as Type (Type (..), smallType)
import Data.Foldable (Foldable (toList))
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Syntax.Tree.Brand as Brand

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
