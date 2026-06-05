{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Instance where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type role Instance nominal nominal nominal

type Instance :: Layout -> Stage -> Environment -> Type
data Instance layout stage scope
