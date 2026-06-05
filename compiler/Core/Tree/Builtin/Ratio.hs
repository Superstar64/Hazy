module Core.Tree.Builtin.Ratio where

import Core.Tree.Constructor (Constructor (..))
import Core.Tree.Data (Data (..))
import Core.Tree.Entry (Entry (..))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Syntax.Tree.Brand as Brand

ratio :: Data scope
ratio =
  Data
    { parameters = Strict.Vector.singleton Type.smallType,
      selectors = Strict.Vector.empty,
      brand = Brand.Boxed,
      constructors =
        Strict.Vector.singleton
          Constructor
            { entries =
                Strict.Vector.fromList
                  [ Entry
                      { entry = a,
                        strict = Type.Constructor Type2.Strict
                      },
                    Entry
                      { entry = a,
                        strict = Type.Constructor Type2.Strict
                      }
                  ]
            }
    }
  where
    a = Type.Variable (Local.Local 0)
