{-# LANGUAGE RoleAnnotations #-}

module Semantic.Index.Type2 where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import {-# SOURCE #-} Semantic.Shift (Functor)
import Prelude hiding (Functor)

type role Index nominal

type Index :: Environment -> Type
data Index scope

instance Functor Index
