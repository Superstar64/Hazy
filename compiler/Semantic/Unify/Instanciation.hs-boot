{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.Instanciation where

import Core.Tree.Instanciation (InstanciationF)
import {-# SOURCE #-} Semantic.Unify.Evidence (Logical)

newtype Instanciation s scope = Instanciationx {runInstanciationx :: InstanciationF (Logical s) scope}
