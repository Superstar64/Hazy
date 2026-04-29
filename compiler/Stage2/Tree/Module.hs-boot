{-# LANGUAGE RoleAnnotations #-}

module Stage2.Tree.Module where

import Data.Kind (Type)
import Stage2.Layout (Layout)

type role Module nominal

type Module :: Layout -> Type
data Module layout
