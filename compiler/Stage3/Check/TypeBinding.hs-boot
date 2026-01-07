{-# LANGUAGE RoleAnnotations #-}

module Stage3.Check.TypeBinding where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import qualified Data.Strict.Maybe as Strict
import Stage2.Scope (Environment ((:+)), Local)
import {-# SOURCE #-} qualified Stage3.Simple.Type as Simple

type role TypeBinding nominal nominal

type TypeBinding :: Type -> Environment -> Type
data TypeBinding s scope

synonym_ :: TypeBinding s scope -> ST s (Strict.Maybe (Simple.Type (Local ':+ scope)))
