module Builtin.Tuple where

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

definition :: Int -> Data scope
definition n =
  Data
    { parameters = Strict.Vector.replicate n Type.smallType,
      constructors = Strict.Vector.fromList $ toList set,
      selectors = Strict.Vector.empty,
      brand = Brand.Boxed
    }
  where
    set = map go [minBound .. maxBound]
    go Constructor.Tuple =
      Constructor
        { entries = Strict.Vector.generate n $ \n ->
            Entry
              { entry = Type.Variable (Local.Local n),
                strict = Type.Constructor Type2.Lazy
              }
        }
