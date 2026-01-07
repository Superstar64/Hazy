module Stage2.Resolve.Functor.Bindings where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..), trifoldMapDefault, trimapDefault)
import Error
  ( constructorNotInScope,
    typeNotInScope,
    variableNotInScope,
  )
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable
  ( Constructor,
    ConstructorIdentifier,
    Variable,
  )
import qualified Stage2.Resolve.Functor.Binding.Constructor as Constructor
import qualified Stage2.Resolve.Functor.Binding.Term as Term
import qualified Stage2.Resolve.Functor.Binding.Type as Type
import Stage2.Resolve.Functor.Same (Same)
import Prelude hiding (map)

data Bindings stable a b c = Bindings
  { terms :: !(Map Variable (Term.Binding a)),
    constructors :: !(Map Constructor (Constructor.Binding b)),
    types :: !(Map ConstructorIdentifier (Type.Binding c)),
    stability :: !stable
  }
  deriving (Show)

unionWith
  stable
  term
  constructor
  typex
  Bindings {terms = terms1, constructors = constructors1, types = types1, stability = stability1}
  Bindings {terms = terms2, constructors = constructors2, types = types2, stability = stability2} =
    Bindings
      { terms = Map.unionWith term terms1 terms2,
        constructors = Map.unionWith constructor constructors1 constructors2,
        types = Map.unionWith typex types1 types2,
        stability = stable stability1 stability2
      }

instance
  ( Same a,
    Same b,
    Same c,
    Semigroup stable
  ) =>
  Semigroup (Bindings stable a b c)
  where
  (<>)
    Bindings {terms = terms1, constructors = constructors1, types = types1, stability = stability1}
    Bindings {terms = terms2, constructors = constructors2, types = types2, stability = stability2} =
      Bindings
        { terms = Map.unionWith (<>) terms1 terms2,
          constructors = Map.unionWith (<>) constructors1 constructors2,
          types = Map.unionWith (<>) types1 types2,
          stability = stability1 <> stability2
        }

instance
  ( Same a,
    Same b,
    Same c,
    Monoid stable
  ) =>
  Monoid (Bindings stable a b c)
  where
  mempty =
    Bindings
      { terms = Map.empty,
        constructors = Map.empty,
        types = Map.empty,
        stability = mempty
      }

instance Trifunctor (Bindings stable) where
  trimap = trimapDefault

instance Trifoldable (Bindings stable) where
  trifoldMap = trifoldMapDefault

instance Tritraversable (Bindings stable) where
  tritraverse f g h Bindings {terms, constructors, types, stability} =
    Bindings
      <$> traverse (traverse f) terms
      <*> traverse (traverse g) constructors
      <*> traverse (traverse h) types
      <*> pure stability

map ::
  (a1 -> a2) ->
  (b1 -> b2) ->
  (c1 -> c2) ->
  Bindings stable a1 b1 c1 ->
  Bindings stable a2 b2 c2
map = trimap

mapWithKey ::
  (Variable -> a1 -> a2) ->
  (Constructor -> b1 -> b2) ->
  (ConstructorIdentifier -> c1 -> c2) ->
  Bindings stable a1 b1 c1 ->
  Bindings stable a2 b2 c2
mapWithKey f g h Bindings {terms, constructors, types, stability} =
  Bindings
    { terms = Map.mapWithKey (fmap . f) terms,
      constructors = Map.mapWithKey (fmap . g) constructors,
      types = Map.mapWithKey (fmap . h) types,
      stability
    }

updateStability :: stable1 -> Bindings stable2 a b c -> Bindings stable1 a b c
updateStability stability Bindings {terms, types, constructors} =
  Bindings
    { terms,
      types,
      constructors,
      stability
    }

infixl 3 !-, !=, !=.

(!-) :: Bindings stable a b c -> Marked Variable Position -> Term.Binding a
Bindings {terms} !- position :@ name
  | Just index <- Map.lookup name terms = index
  | otherwise = variableNotInScope position

(!=) :: Bindings stable a b c -> Marked Constructor Position -> Constructor.Binding b
Bindings {constructors} != position :@ name
  | Just index <- Map.lookup name constructors = index
  | otherwise = constructorNotInScope position

(!=.) :: Bindings stable a b c -> Marked ConstructorIdentifier Position -> Type.Binding c
Bindings {types} !=. position :@ name
  | Just index <- Map.lookup name types = index
  | otherwise = typeNotInScope position
