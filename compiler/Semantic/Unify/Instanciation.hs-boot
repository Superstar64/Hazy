{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Instanciation where

import Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} Semantic.Unify.Evidence (Logical)

type Instanciation s scope = InstanciationF (Logical s scope) scope
