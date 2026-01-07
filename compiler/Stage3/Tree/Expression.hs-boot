{-# LANGUAGE RoleAnnotations #-}

module Stage3.Tree.Expression where

import Data.Kind (Type)
import qualified Stage2.Scope as Scope (Environment)

type role Expression nominal

type Expression :: Scope.Environment -> Type
data Expression scope

instance Show (Expression scope)
