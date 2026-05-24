{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Instance where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Scope (Environment)
import Stage2.Stage (Stage)

type role Instance nominal nominal nominal

type Instance :: Layout -> Stage -> Environment -> Type
data Instance layout stage scope
