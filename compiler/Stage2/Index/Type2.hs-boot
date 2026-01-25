{-# LANGUAGE RoleAnnotations #-}

module Stage2.Index.Type2 where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import {-# SOURCE #-} Stage2.Shift (Functor)
import Prelude hiding (Functor)

type role Index nominal

type Index :: Environment -> Type
data Index scope

instance Functor Index
