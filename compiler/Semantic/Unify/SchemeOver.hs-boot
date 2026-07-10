{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify.SchemeOver where

import Core.Tree.SchemeOver (SchemeOverF)
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

newtype SchemeOver typex s scope = SchemeOverx {runSchemeOverx :: SchemeOverF (Logical s) (typex s) scope}
