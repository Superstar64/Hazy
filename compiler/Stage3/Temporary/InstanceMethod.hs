module Stage3.Temporary.InstanceMethod where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..), Local)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Tree.InstanceMethod as Solved
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Constraint as Simple (Constraint)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.Type as Simple (Type)

data InstanceMethod s scope
  = Definition
      { parameters :: !(Strict.Vector (Simple.Type (Local ':+ scope))),
        constraints :: !(Strict.Vector (Simple.Constraint (Local ':+ scope))),
        definition :: !(Definition s (Local ':+ Local ':+ scope))
      }
  | Default
      { parameters :: !(Strict.Vector (Simple.Type (Local ':+ scope))),
        constraints :: !(Strict.Vector (Simple.Constraint (Local ':+ scope))),
        base :: !(Simple.Type (Local ':+ scope)),
        self :: !(Simple.Evidence (Local ':+ scope)),
        defaultx :: !(Unify.Delay Simple.Expression s (Local ':+ (Local ':+ scope)))
      }

instance Unify.Zonk InstanceMethod where
  zonk zonker = \case
    Definition {parameters, constraints, definition} -> do
      definition <- Unify.zonk zonker definition
      pure Definition {parameters, constraints, definition}
    Default {parameters, constraints, base, self, defaultx} -> do
      defaultx <- Unify.zonk zonker defaultx
      pure Default {parameters, constraints, base, self, defaultx}

solve :: InstanceMethod s scope -> ST s (Solved.InstanceMethod scope)
solve = \case
  Definition {parameters, constraints, definition} -> do
    definition <- Definition.solve definition
    pure Solved.Definition {parameters, constraints, definition}
  Default {parameters, constraints, base, self, defaultx = Unify.Delay defaultx} -> do
    defaultx <- defaultx
    pure Solved.Default {parameters, constraints, base, self, defaultx}
