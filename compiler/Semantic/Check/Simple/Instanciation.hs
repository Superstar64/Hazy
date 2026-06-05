module Semantic.Check.Simple.Instanciation where

import Core.Tree.Instanciation (Instanciation (..))
import {-# SOURCE #-} Semantic.Check.Simple.Evidence as Evidence (lift)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

lift :: Instanciation scope -> Unify.Instanciation s scope
lift (Instanciation instanciation) = Unify.instanciation (Evidence.lift <$> instanciation)
