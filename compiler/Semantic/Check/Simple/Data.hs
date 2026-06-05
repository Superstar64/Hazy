module Semantic.Check.Simple.Data where

import Control.Monad.ST (ST)
import Core.Tree.Data (Data (..))
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (DataInstance))
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Constructor as Constructor
import qualified Semantic.Check.Simple.Type as Type
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

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
