module Semantic.Resolve.Bindings where

import Data.Map (Map)
import qualified Data.Map as Map
import Error (constructorNotInScope, typeNotInScope, unstableShadowing, variableNotInScope)
import qualified Semantic.Resolve.Binding.Constructor as Constructor
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import qualified Semantic.Resolve.Detail.Binding.Constructor as Detail.Constructor
import qualified Semantic.Resolve.Detail.Binding.Term as Detail.Term
import qualified Semantic.Resolve.Detail.Binding.Type as Detail.Type
import qualified Semantic.Resolve.Functor.Bindings as Functor
import Semantic.Resolve.Stability (Stability (..))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Syntax.Lexer (ConstructorIdentifier)
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (Constructor, Variable)

data Bindings stability scope = Bindings
  { terms :: !(Map Variable (Term.Binding scope)),
    constructors :: !(Map Constructor (Constructor.Binding scope)),
    types :: !(Map ConstructorIdentifier (Type.Binding scope)),
    stability :: !stability
  }
  deriving (Show)

instance Shift (Bindings stability) where
  shift = shiftDefault

instance Shift.Functor (Bindings stability) where
  map category Bindings {terms, constructors, types, stability} =
    Bindings
      { terms = fmap (Shift.map category) terms,
        constructors = fmap (Shift.map category) constructors,
        types = fmap (Shift.map category) types,
        stability
      }

instance (Semigroup stability) => Semigroup (Bindings stability scope) where
  (<>)
    Bindings {terms = terms1, constructors = constructors1, types = types1, stability = stability1}
    Bindings {terms = terms2, constructors = constructors2, types = types2, stability = stability2} =
      Bindings
        { terms = Map.unionWith (<>) terms1 terms2,
          constructors = Map.unionWith (<>) constructors1 constructors2,
          types = Map.unionWith (<>) types1 types2,
          stability = stability1 <> stability2
        }

instance (Monoid stability) => Monoid (Bindings stability scope) where
  mempty =
    Bindings
      { terms = Map.empty,
        constructors = Map.empty,
        types = Map.empty,
        stability = mempty
      }

infixr 6 </>

(</>) :: Bindings Stability scope -> Bindings Stability scope -> Bindings Stability scope
(</>)
  Bindings {terms = terms1, constructors = constructors1, types = types1, stability = stability1}
  Bindings {terms = terms2, constructors = constructors2, types = types2, stability = stability}
    | Unstable position <- stability1 = case stability of
        Ignore -> Bindings {terms, constructors, types, stability}
        _ -> unstableShadowing position
    | otherwise = Bindings {terms, constructors, types, stability}
    where
      terms = Map.unionWith combine terms1 terms2
        where
          combine left@Term.Binding {selector} ~Term.Binding {selector = selector'} =
            left {Term.selector = selector <> selector'}
      constructors = Map.union constructors1 constructors2
      types = Map.union types1 types2

infixl 3 !-, !=, !=.

Bindings {terms} !- position :@ name
  | Just index <- Map.lookup name terms = index
  | otherwise = variableNotInScope position

Bindings {constructors} != position :@ name
  | Just index <- Map.lookup name constructors = index
  | otherwise = constructorNotInScope position

Bindings {types} !=. position :@ name
  | Just index <- Map.lookup name types = index
  | otherwise = typeNotInScope position

fromFunctor ::
  Functor.Bindings
    stability
    (Detail.Term.Binding scope)
    (Detail.Constructor.Binding scope)
    (Detail.Type.Binding scope) ->
  Bindings stability scope
fromFunctor
  Functor.Bindings
    { terms,
      constructors,
      types,
      stability
    } =
    Bindings
      { terms = fmap Term.fromFunctor terms,
        constructors = fmap Constructor.fromFunctor constructors,
        types = fmap Type.fromFunctor types,
        stability
      }

toFunctor ::
  Bindings stable scope ->
  Functor.Bindings
    stable
    (Detail.Term.Binding scope)
    (Detail.Constructor.Binding scope)
    (Detail.Type.Binding scope)
toFunctor
  Bindings
    { terms,
      constructors,
      types,
      stability
    } =
    Functor.Bindings
      { terms = fmap Term.toFunctor terms,
        constructors = fmap Constructor.toFunctor constructors,
        types = fmap Type.toFunctor types,
        stability
      }

updateStability stability Bindings {terms, constructors, types} =
  Bindings
    { terms,
      constructors,
      types,
      stability
    }
