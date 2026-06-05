module Stage2.Check.Simple.Data where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Check.Context (Context)
import Stage2.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage2.Check.DataInstance as DataInstance
import qualified Stage2.Check.Simple.Constructor as Constructor
import qualified Stage2.Check.Simple.Type as Type
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import Stage4.Tree.Data (Data (..))

instanciate :: Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciate context position Data {parameters, constructors, selectors, brand} = do
  types <- traverse (Unify.fresh . Type.lift) parameters
  let datax =
        DataInstance
          { position,
            types,
            selectors,
            constructors = Constructor.instanciate position brand types <$> constructors
          }
  DataInstance.mark context datax
  pure datax
