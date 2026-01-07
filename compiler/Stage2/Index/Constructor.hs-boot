{-# LANGUAGE RoleAnnotations #-}

module Stage2.Index.Constructor where

import Data.Kind (Type)
import qualified Stage2.Index.Type as Type1
import Stage2.Scope (Environment)

type role Index nominal

type Index :: Environment -> Type
data Index scope

instance Show (Index scope)

instance Eq (Index scope)

instance Ord (Index scope)

traverse :: (Applicative m) => (Type1.Index scope -> m (Type1.Index scope')) -> Index scope -> m (Index scope')
