module Stage3.Simple.Data where

import Control.Monad.ST (ST)
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import qualified Stage3.Simple.Constructor as Constructor
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Data (Data (..))

instanciate :: Data scope -> ST s (DataInstance s scope)
instanciate Data {parameters, constructors, selectors} = do
  types <- traverse (Unify.fresh . Type.lift) parameters
  pure
    DataInstance
      { types,
        constructors = Constructor.instanciate types <$> constructors,
        selectors
      }
