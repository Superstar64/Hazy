{-# LANGUAGE RoleAnnotations #-}

module Semantic.Check.TypeBinding where

import Control.Monad.ST (ST)
import {-# SOURCE #-} qualified Core.Tree.Type as Simple
import Data.Kind (Type)
import qualified Data.Strict.Maybe as Strict
import Semantic.Scope (Environment ((:+)), Local)

type role TypeBinding nominal nominal

type TypeBinding :: Type -> Environment -> Type
data TypeBinding s scope

synonym_ :: TypeBinding s scope -> ST s (Strict.Maybe (Simple.Type (Local ':+ scope)))
