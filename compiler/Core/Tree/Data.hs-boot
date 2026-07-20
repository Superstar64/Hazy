{-# LANGUAGE RoleAnnotations #-}

module Core.Tree.Data where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type role Data nominal

type Data :: Environment -> Type
data Data scope

instance Shift0.Functor Data

instance Shift.Functor Data
