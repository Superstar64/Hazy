module Stage3.Simple.Evidence where

import qualified Stage3.Simple.Instanciation as Instanciation (lift)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Evidence (Evidence (..))

lift :: Evidence scope -> Unify.Evidence s scope
lift = \case
  Variable {variable, instanciation} -> Unify.variable' variable (Instanciation.lift instanciation)
  Super {base, index} -> Unify.super (lift base) index
