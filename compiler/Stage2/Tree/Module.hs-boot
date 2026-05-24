{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Module where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Stage (Stage)

type role Module nominal nominal

type Module :: Layout -> Stage -> Type
data Module layout stage
