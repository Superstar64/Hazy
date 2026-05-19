{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Declarations where

import Data.Kind (Type)
import Stage2.Layout (Layout)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)

type role Declarations nominal nominal nominal

type Declarations :: Locality -> Layout -> Environment -> Type
data Declarations locality layout scope

instance Show (Declarations locality layout scope)
