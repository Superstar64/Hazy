module Semantic.Resolve.Functor.Canonical where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..), trifoldMapDefault, trimapDefault)
import Error (moduleNotFound)
import Semantic.Resolve.Functor.Bindings (Bindings)
import qualified Semantic.Resolve.Functor.Bindings as Bindings
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable
  ( FullQualifiers,
    FullyQualifiedConstructor (..),
    FullyQualifiedConstructorIdentifier (..),
    FullyQualifiedVariable (..),
  )
import Prelude hiding (map)

newtype Canonical a b c = Canonical {runCanonical :: Map FullQualifiers (Bindings () a b c)}

empty = Canonical Map.empty

instance Trifunctor Canonical where
  trimap = trimapDefault

instance Trifoldable Canonical where
  trifoldMap = trifoldMapDefault

instance Tritraversable Canonical where
  tritraverse f g h (Canonical canonical) = Canonical <$> traverse (tritraverse f g h) canonical

map ::
  (a1 -> a2) ->
  (b1 -> b2) ->
  (c1 -> c2) ->
  Canonical a1 b1 c1 ->
  Canonical a2 b2 c2
map = trimap

mapWithKey ::
  (FullyQualifiedVariable -> a1 -> a2) ->
  (FullyQualifiedConstructor -> b1 -> b2) ->
  (FullyQualifiedConstructorIdentifier -> c1 -> c2) ->
  Canonical a1 b1 c1 ->
  Canonical a2 b2 c2
mapWithKey f g h (Canonical canonical) =
  Canonical $
    Map.mapWithKey
      ( \k ->
          Bindings.mapWithKey
            (f . (:.-) k)
            (g . (:.=) k)
            (h . (:.=.) k)
      )
      canonical

infixl 3 !

(!) :: Canonical a b c -> Marked FullQualifiers Position -> Bindings () a b c
Canonical table ! position :@ name = case Map.lookup name table of
  Just bindings -> bindings
  Nothing -> moduleNotFound position
