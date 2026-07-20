module Core.Tree.Class where

import {-# SOURCE #-} Core.Tree.Constraint (Constraint)
import {-# SOURCE #-} Core.Tree.Forall (Forall)
import {-# SOURCE #-} Core.Tree.Type (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Forall (Local ':+ scope)))
  }

instance Shift0.Functor Class

instance Shift.Functor Class

kind :: Class scope -> Type scope
