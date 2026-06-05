module Semantic.Check.LocalBinding where

import qualified Core.Tree.Evidence as Simple (Evidence)
import Core.Tree.Type as Simple (Type)
import qualified Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict
import Error (nonUniqueConstraints)
import Semantic.Check.Mask (Mask)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Local as Label
import Semantic.Scope (Environment)
import Semantic.Shift (Shift (shift))
import {-# SOURCE #-} qualified Semantic.Unify as Unify (Type)
import Syntax.Position (Position)

type LocalBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data LocalBinding s scope
  = Rigid
      { label :: !(forall scope. Label.LocalBinding scope),
        rigid :: !(Simple.Type scope),
        constraints :: !(Map (Type2.Index scope) (Constraint scope)),
        mask :: !Mask
      }
  | Wobbly
      { label :: !(forall scope. Label.LocalBinding scope),
        wobbly :: !(Unify.Type s scope)
      }

instance Shift (LocalBinding s) where
  shift = \case
    Rigid {label, rigid, constraints, mask} ->
      Rigid
        { label,
          rigid = shift rigid,
          constraints = Map.map shift $ Map.mapKeysMonotonic shift constraints,
          mask
        }
    Wobbly {label, wobbly} -> Wobbly {label, wobbly = shift wobbly}

data Constraint scope = Constraint
  { arguments :: !(Strict.Vector (Simple.Type scope)),
    evidence :: !(Simple.Evidence scope)
  }
  deriving (Show)

combine :: Position -> Constraint scope -> Constraint scope -> Constraint scope
combine position left@Constraint {arguments} Constraint {arguments = argument'}
  -- evidence is left biased
  | arguments == argument' = left
  | otherwise = nonUniqueConstraints position

instance Shift Constraint where
  shift Constraint {arguments, evidence} =
    Constraint
      { arguments = fmap shift arguments,
        evidence = shift evidence
      }
