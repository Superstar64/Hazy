module Semantic.Check.Simple.Evidence where

import {-# SOURCE #-} Core.Tree.Evidence (Evidence)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

lift :: Evidence scope -> Unify.Evidence s scope
