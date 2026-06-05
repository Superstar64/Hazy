module Core.Tree.Class where

import {-# SOURCE #-} Core.Tree.Constraint (Constraint)
import {-# SOURCE #-} Core.Tree.Scheme (Scheme)
import {-# SOURCE #-} Core.Tree.Type (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }

instance Shift Class

instance Shift.Functor Class

kind :: Class scope -> Type scope
