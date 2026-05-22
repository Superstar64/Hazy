module Stage3.Tree.Declaration where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import Stage2.Tree.Declaration (Key (..))
import Stage3.Tree.Definition4 (Definition4 (..))
import qualified Stage4.Tree.Scheme as Simple (Scheme)
import Prelude hiding (Maybe (Just))

type Declaration :: Locality -> Layout -> Environment -> Type
data Declaration locality layout scope
  = Declaration
  { name :: !Key,
    definition :: Definition4 scope,
    typex :: Simple.Scheme scope
  }
  deriving (Show)

lazy :: Key -> Declaration locality layout scope -> Declaration locality layout scope
lazy name ~Declaration {definition, typex} =
  Declaration
    { name,
      definition,
      typex
    }

typex_ :: Declaration locality layout scope -> Simple.Scheme scope
typex_ = typex
