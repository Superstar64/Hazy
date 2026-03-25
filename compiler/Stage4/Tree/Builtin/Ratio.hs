module Stage4.Tree.Builtin.Ratio where

import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage1.Tree.Brand as Brand
import qualified Stage2.Index.Local as Local
import Stage4.Tree.Constructor (Constructor (..))
import Stage4.Tree.Data (Data (..))
import Stage4.Tree.Entry (Entry (..))
import qualified Stage4.Tree.Type as Type

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
                        strict = True
                      },
                    Entry
                      { entry = a,
                        strict = True
                      }
                  ]
            }
    }
  where
    a = Type.Variable (Local.Local 0)
