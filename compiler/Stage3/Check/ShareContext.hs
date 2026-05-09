module Stage3.Check.ShareContext where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Index.Table.Term as Term
import Stage2.Scope (Declaration, Environment (..), Global)
import Stage3.Check.Context (Context (..))
import Stage3.Check.TermBinding (TermBinding)

newtype ShareContext s scope = ShareContext
  { shared :: Vector (TermBinding s scope)
  }

(!) :: ShareContext s scope -> Int -> TermBinding s scope
ShareContext {shared} ! index = shared Vector.! index

localBindings :: Context s (Declaration ':+ scope) -> ShareContext s (Declaration ':+ scope)
localBindings Context {termEnvironment = Term.Declaration shared _} = ShareContext {shared}

globalBindings :: Context s Global -> Int -> ShareContext s Global
globalBindings Context {termEnvironment = Term.Global globals} index =
  ShareContext
    { shared = globals Vector.! index
    }
