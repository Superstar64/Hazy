{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Module where

import Data.Kind (Type)
import Semantic.Layout (Layout)
import Semantic.Stage (Stage)

type role Module nominal nominal

type Module :: Layout -> Stage -> Type
data Module layout stage
