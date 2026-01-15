module Stage4.Tree.Class where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage4.Tree.Constraint (Constraint)
import {-# SOURCE #-} Stage4.Tree.Scheme (Scheme)
import {-# SOURCE #-} Stage4.Tree.Type (Type)

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }

instance Shift Class

instance Shift.Functor Class

kind :: Class scope -> Type scope
