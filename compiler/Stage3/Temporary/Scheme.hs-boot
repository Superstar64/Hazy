{-# LANGUAGE RoleAnnotations #-}

module Stage3.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Stage1.Position (Position)
import Stage2.Scope (Environment)
import qualified Stage2.Tree.Scheme as Stage2
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Synonym as Synonym
import {-# SOURCE #-} qualified Stage3.Tree.Scheme as Solved

type role Scheme nominal nominal

type Scheme :: Type -> Environment -> Type
data Scheme s scope

check :: Context s scope -> Stage2.Scheme Position scope -> ST s (Scheme s scope)
solve :: Synonym.Context s scope -> Scheme s scope -> ST s (Solved.Scheme scope)
