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
import qualified Semantic.Shift0 as Shift0
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

instance Shift0.Functor (LocalBinding s) where
  map category = \case
    Rigid {label, rigid, constraints, mask} ->
      Rigid
        { label,
          rigid = Shift0.map category rigid,
          constraints = Map.map (Shift0.map category) $ Map.mapKeysMonotonic (Shift0.map category) constraints,
          mask
        }
    Wobbly {label, wobbly} -> Wobbly {label, wobbly = Shift0.map category wobbly}

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

instance Shift0.Functor Constraint where
  map category Constraint {arguments, evidence} =
    Constraint
      { arguments = fmap (Shift0.map category) arguments,
        evidence = (Shift0.map category) evidence
      }
