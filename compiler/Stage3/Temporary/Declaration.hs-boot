{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Declaration where

import Data.Kind (Type)
import Stage2.Locality (Locality)
import Stage2.Scope (Environment)
import {-# SOURCE #-} qualified Stage3.Unify as Unify

type role Declaration nominal nominal nominal

type Declaration :: Locality -> Type -> Environment -> Type
data Declaration locality s scope

typex' :: Declaration locality s scope -> Unify.Scheme s scope
