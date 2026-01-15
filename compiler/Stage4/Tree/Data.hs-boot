{-# LANGUAGE RoleAnnotations #-}

module Stage4.Tree.Data where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift

type role Data nominal

type Data :: Environment -> Type
data Data scope

instance Shift Data

instance Shift.Functor Data
