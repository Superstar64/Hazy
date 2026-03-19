module Stage3.Simple.Instanciation where

import {-# SOURCE #-} Stage3.Simple.Evidence as Evidence (lift)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Instanciation (Instanciation (..))

lift :: Instanciation scope -> Unify.Instanciation s scope
lift (Instanciation instanciation) = Unify.instanciation (Evidence.lift <$> instanciation)
