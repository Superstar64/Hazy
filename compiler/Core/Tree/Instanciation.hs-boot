{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Instanciation where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import {-# SOURCE #-} Core.Tree.Evidence (Evidence)
import qualified Data.Vector.Strict as Strict
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift

data Instanciation scope
  = Instanciation !(Strict.Vector (Evidence scope))
  | Mono

instance Show (Instanciation scope)

instance Shift Instanciation

instance Shift.Functor Instanciation

instance Shift2.Functor Instanciation

instance Substitute.Functor Instanciation
