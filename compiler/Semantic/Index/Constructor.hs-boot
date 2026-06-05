{-# LANGUAGE RoleAnnotations #-}

module Semantic.Index.Constructor where

import Data.Kind (Type)
import qualified Semantic.Index.Type as Type1
import Semantic.Scope (Environment)

type role Index nominal

type Index :: Environment -> Type
data Index scope

instance Show (Index scope)

instance Eq (Index scope)

instance Ord (Index scope)

traverse :: (Applicative m) => (Type1.Index scope -> m (Type1.Index scope')) -> Index scope -> m (Index scope')
