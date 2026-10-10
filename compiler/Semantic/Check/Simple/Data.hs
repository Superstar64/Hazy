module Semantic.Check.Simple.Data where

import Control.Monad.ST (ST)
import Core.Instanciate (Store (..))
import Core.Substitute (logicalType)
import Core.Tree.Combinators.Delay (Delay (..))
import Core.Tree.Data (Data (..), DefinitionF (..), Types (..))
import qualified Core.Tree.Data as Data
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance)
import qualified Semantic.Check.DataInstance as DataInstance
import qualified Semantic.Check.Simple.Constructor as Constructor
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

instanciate, instanciateMark :: Context s scope -> Position -> Data scope -> ST s (DataInstance s scope)
instanciate
  _
  position
  Data
    { parameters,
      definition = Definition {constructors, selectors, brand}
    } = do
    types <- traverse (Unify.fresh . logicalType) parameters
    let datax =
          Data.Definition
            { position = Store position,
              types = Delay (Types types),
              selectors,
              constructors = Constructor.instanciate position brand types <$> constructors,
              brand
            }
    pure datax
instanciateMark context position datax = do
  datax <- instanciate context position datax
  DataInstance.mark context datax
  pure datax
