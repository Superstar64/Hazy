{-# LANGUAGE RoleAnnotations #-}

module Stage3.Simple.Type where

import qualified Data.Kind as Kind
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type role Type nominal

type Type :: Environment -> Kind.Type
data Type scope

instance Show (Type scope)

instance Shift Type

instance Shift.Functor Type
