module Stage3.Simple.Class where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage3.Simple.Constraint (Constraint)
import {-# SOURCE #-} Stage3.Simple.Scheme (Scheme)
import {-# SOURCE #-} Stage3.Simple.Type (Type)

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }

instance Shift Class

instance Shift.Functor Class
