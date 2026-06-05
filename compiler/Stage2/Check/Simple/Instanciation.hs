module Stage2.Check.Simple.Instanciation where

import {-# SOURCE #-} Stage2.Check.Simple.Evidence as Evidence (lift)
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import Stage4.Tree.Instanciation (Instanciation (..))

lift :: Instanciation scope -> Unify.Instanciation s scope
lift (Instanciation instanciation) = Unify.instanciation (Evidence.lift <$> instanciation)
