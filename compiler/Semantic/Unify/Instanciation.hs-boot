{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Instanciation where

import Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} Semantic.Unify.Evidence (Logical)
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type

type Instanciation s scope = InstanciationF (Logical s scope) (Type.Logical s scope) scope
