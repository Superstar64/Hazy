module Semantic.Check.Simple.Evidence where

import Core.Tree.Evidence (Evidence (..))
import qualified Semantic.Check.Simple.Instanciation as Instanciation (lift)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

lift :: Evidence scope -> Unify.Evidence s scope
lift = \case
  Variable {variable, instanciation} -> Unify.variable' variable (Instanciation.lift instanciation)
  Super {base, index} -> Unify.super (lift base) index
