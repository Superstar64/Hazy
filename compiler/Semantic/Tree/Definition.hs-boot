{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Definition where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import Semantic.Stage (Stage)

type Definition :: Layout -> Stage -> Environment -> Type

type role Definition nominal nominal nominal

data Definition layout stage scope
