{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.Temporary.Declaration where

import Data.Kind (Type)
import Semantic.Locality (Locality)
import Semantic.Scope (Environment)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

type role Declaration nominal nominal nominal

type Declaration :: Locality -> Type -> Environment -> Type
data Declaration locality s scope

typex' :: Declaration locality s scope -> Unify.Scheme s scope
