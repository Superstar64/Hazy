module Stage2.Resolve.Functor.Core
  ( Core (..),
    map,
    (!),
    (!-),
    (!=),
    (!=.),
    updateStability,
    fromMap,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trifunctor (Trifunctor (..))
import Error (moduleNotInScope)
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable
  ( FullQualifiers (..),
    QualifiedConstructor (..),
    QualifiedConstructorIdentifier (..),
    QualifiedVariable (..),
    Qualifiers (..),
  )
import qualified Stage2.Resolve.Functor.Binding.Constructor as Constructor
import qualified Stage2.Resolve.Functor.Binding.Term as Term
import qualified Stage2.Resolve.Functor.Binding.Type as Type
import Stage2.Resolve.Functor.Bindings (Bindings (Bindings))
import qualified Stage2.Resolve.Functor.Bindings as Bindings
import Stage2.Resolve.Stability (Stability (Ignore))
import Prelude hiding (map)

data Core stable a b c = Core
  { globals :: !(Map FullQualifiers (Bindings stable a b c)),
    locals :: !(Bindings stable a b c)
  }
  deriving (Show)

instance Trifunctor (Core stable) where
  trimap f g h Core {globals, locals} =
    Core
      { globals = fmap (Bindings.map f g h) globals,
        locals = Bindings.map f g h locals
      }

map ::
  (a1 -> b1) ->
  (a2 -> b2) ->
  (a3 -> b3) ->
  Core stable a1 a2 a3 ->
  Core stable b1 b2 b3
map = trimap

infixl 3 !, !-, !=, !=.

(!) :: Core stable a b c -> Marked Qualifiers Position -> Bindings stable a b c
Core {locals} ! _ :@ Local = locals
Core {globals} ! position :@ qualifiers :. name
  | Just bindings <- Map.lookup (qualifiers :.. name) globals = bindings
  | otherwise = moduleNotInScope position

(!-) :: Core stable a b c -> Marked QualifiedVariable Position -> Term.Binding a
core !- position :@ qualifiers :- name = core ! position :@ qualifiers Bindings.!- position :@ name

(!=) :: Core stable a b c -> Marked QualifiedConstructor Position -> Constructor.Binding b
core != position :@ qualifiers := name = core ! position :@ qualifiers Bindings.!= position :@ name

(!=.) :: Core stable a b c -> Marked QualifiedConstructorIdentifier Position -> Type.Binding c
core !=. position :@ qualifiers :=. name = core ! position :@ qualifiers Bindings.!=. position :@ name

updateStability :: stable -> Core stable' a b c -> Core stable a b c
updateStability stability Core {locals, globals} =
  Core
    { locals = Bindings.updateStability stability locals,
      globals = Map.map (Bindings.updateStability stability) globals
    }

fromMap :: Map Qualifiers (Bindings Stability a b c) -> Core Stability a b c
fromMap core = Core {locals, globals}
  where
    locals = Map.findWithDefault unbound Local core
    unbound =
      Bindings
        { terms = Map.empty,
          constructors = Map.empty,
          types = Map.empty,
          stability = Ignore
        }
    globals = Map.mapKeysMonotonic full $ Map.delete Local core
      where
        full (root :. name) = root :.. name
        full Local = error "map local"
