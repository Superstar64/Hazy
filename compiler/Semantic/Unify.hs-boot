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
    fresh,
    mark,
    solve,
    unify,
  )
where

import Control.Monad.ST (ST)
import Core.Tree.Forall (ForallOver)
import {-# SOURCE #-} Semantic.Unify.Constraint hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Forall (Forall, instanciate)
import {-# SOURCE #-} Semantic.Unify.Generalizable (Generalizable (..))
import {-# SOURCE #-} Semantic.Unify.Instanciation hiding (solve, unify)
import Semantic.Unify.Solve (Solve)
import {-# SOURCE #-} Semantic.Unify.Type
import {-# SOURCE #-} Semantic.Unify.Zonk (Zonk (..))

liftST :: ST s a -> Solve s a
runSolve :: Solve s a -> ST s a
