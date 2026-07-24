-- |
-- Unification public api
module Semantic.Unify
  ( Logical,
    Type,
    Forall,
    ForallOver,
    Constraints,
    Constraint,
    Evidence,
    Instanciation,
    fresh,
    mark,
    unify,
    constrain,
    Zonk (..),
    Zonker,
    Generalizable (..),
    Generalize (..),
    Body ((:::)),
    instanciate,
    generalizeBody,
    Solve,
    liftST,
    runSolve,
    SolveType (..),
    SolveEvidence (..),
    MapForall (..),
    mapForall,
  )
where

import Core.Tree.Forall (ForallOver)
import Semantic.Unify.Constraint (Constraint)
import Semantic.Unify.Constraints (Constraints)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Forall
  ( Body ((:::)),
    Forall,
    Generalize (..),
    MapForall (..),
    generalizeBody,
    instanciate,
    mapForall,
  )
import {-# SOURCE #-} Semantic.Unify.Generalizable
  ( Generalizable (..),
  )
import Semantic.Unify.Instanciation
  ( Instanciation,
  )
import {-# SOURCE #-} Semantic.Unify.Solve
  ( Solve,
    SolveEvidence (..),
    SolveType (..),
    liftST,
    runSolve,
  )
import {-# SOURCE #-} Semantic.Unify.Type
  ( Logical,
    Type,
    constrain,
    fresh,
    mark,
    unify,
  )
import {-# SOURCE #-} Semantic.Unify.Zonk
  ( Zonk (..),
    Zonker,
  )
