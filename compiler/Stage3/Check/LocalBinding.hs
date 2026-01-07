module Stage3.Check.LocalBinding where

import qualified Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Local as Label
import Stage2.Scope (Environment)
import Stage2.Shift (Shift (shift))
import qualified Stage3.Simple.Evidence as Simple (Evidence)
import Stage3.Simple.Type as Simple (Type)
import {-# SOURCE #-} qualified Stage3.Unify as Unify (Type)

type LocalBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data LocalBinding s scope
  = Rigid
      { label :: !(forall scope. Label.LocalBinding scope),
        rigid :: !(Simple.Type scope),
        constraints :: Map (Type2.Index scope) (Constraint scope)
      }
  | Wobbly
      { label :: !(forall scope. Label.LocalBinding scope),
        wobbly :: !(Unify.Type s scope)
      }

instance Shift (LocalBinding s) where
  shift = \case
    Rigid {label, rigid, constraints} ->
      Rigid
        { label,
          rigid = shift rigid,
          constraints = Map.map shift $ Map.mapKeysMonotonic shift constraints
        }
    Wobbly {label, wobbly} -> Wobbly {label, wobbly = shift wobbly}

data Constraint scope = Constraint
  { arguments :: !(Strict.Vector (Simple.Type scope)),
    evidence :: !(Simple.Evidence scope)
  }

instance Shift Constraint where
  shift Constraint {arguments, evidence} =
    Constraint
      { arguments = fmap shift arguments,
        evidence = shift evidence
      }
