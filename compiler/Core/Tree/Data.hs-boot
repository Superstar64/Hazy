{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Data where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift)
import qualified Semantic.Shift as Shift

type role Data nominal

type Data :: Environment -> Type
data Data scope

instance Shift Data

instance Shift.Functor Data
