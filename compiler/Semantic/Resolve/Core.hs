module Semantic.Resolve.Core where

import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Error (moduleNotInScope)
import Semantic.Resolve.Bindings (BindingsF (..))
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Stability (Stability (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable
  ( FullQualifiers ((:..)),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier ((:=.)),
    QualifiedVariable (..),
    Qualifiers (..),
  )

type Core = CoreF Identity

data CoreF m scope = Core
  { globals :: !(Map FullQualifiers (BindingsF Stability m scope)),
    locals :: !(BindingsF Stability m scope)
  }
  deriving (Show)

instance (Applicative m) => Semigroup (CoreF m scope) where
  Core {globals = globals1, locals = locals1}
    <> Core {globals = globals2, locals = locals2} =
      Core
        { globals = Map.unionWith (<>) globals1 globals2,
          locals = locals1 <> locals2
        }

infixl 3 !, !-, !=, !=.

Core {locals} ! _ :@ Local = locals
Core {globals} ! position :@ qualifiers :. name
  | Just bindings <- Map.lookup (qualifiers :.. name) globals = bindings
  | otherwise = moduleNotInScope position

core !- position :@ qualifiers :- name = core ! position :@ qualifiers Bindings.!- position :@ name

core != position :@ qualifiers := name = core ! position :@ qualifiers Bindings.!= position :@ name

core !=. position :@ qualifiers :=. name = core ! position :@ qualifiers Bindings.!=. position :@ name

updateStability stability Core {locals, globals} =
  Core
    { locals = Bindings.updateStability stability locals,
      globals = Map.map (Bindings.updateStability stability) globals
    }

infixr 5 </>

(</>) :: Core scope -> Core scope -> Core scope
Core {globals = globals1, locals = locals1} </> Core {globals = globals2, locals = locals2} =
  Core
    { globals = Map.unionWith (Bindings.</>) globals1 globals2,
      locals = locals1 Bindings.</> locals2
    }

fromMap map =
  Core
    { locals = Map.findWithDefault unbound Local map,
      globals = Map.mapKeysMonotonic assume $ Map.delete Local map
    }
  where
    unbound =
      Bindings
        { terms = Map.empty,
          constructors = Map.empty,
          types = Map.empty,
          stability = Ignore
        }
    assume :: Qualifiers -> FullQualifiers
    assume (root :. name) = root :.. name
    assume Local = error "bad assume"
