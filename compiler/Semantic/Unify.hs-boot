{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify
  ( module Semantic.Unify,
    Evidence,
    Zonk (..),
    Generalizable (..),
    Constraints,
    Constraint,
    Instanciation,
    Forall,
    ForallOver,
    Type,
    Solve,
    instanciate,
  )
where

import Control.Monad.ST (ST)
import Core.Tree.Forall (ForallOver)
import qualified Core.Tree.Type as Simple
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Check.Mask as Mask
import {-# SOURCE #-} Semantic.Unify.Constraint hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Forall (Forall, instanciate)
import {-# SOURCE #-} Semantic.Unify.Generalizable (Generalizable (..))
import {-# SOURCE #-} Semantic.Unify.Instanciation hiding (solve, unify)
import Semantic.Unify.Solve (Solve)
import {-# SOURCE #-} Semantic.Unify.Type
import {-# SOURCE #-} Semantic.Unify.Zonk (Zonk (..))
import Syntax.Position (Position)

liftST :: ST s a -> Solve s a
runSolve :: Solve s a -> ST s a
fresh :: Type s scope -> ST s (Type s scope)
mark :: Context s scope -> Position -> Mask.Erasure -> Type s scope -> ST s ()
solve :: Position -> Type s scope -> Solve s (Simple.Type scope)
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
