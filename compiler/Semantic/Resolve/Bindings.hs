module Semantic.Resolve.Bindings where

import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Error (constructorNotInScope, typeNotInScope, unstableShadowing, variableNotInScope)
import qualified Semantic.Resolve.Binding.Constructor as Constructor
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Semantic.Resolve.Functor2 (Traversable2 (..))
import Semantic.Resolve.Stability (Stability (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Syntax.Lexer (ConstructorIdentifier)
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (Constructor, Variable)

type Bindings stability = BindingsF stability Identity

data BindingsF stability m scope = Bindings
  { terms :: !(Map Variable (Term.BindingF m scope)),
    constructors :: !(Map Constructor (Constructor.BindingF m scope)),
    types :: !(Map ConstructorIdentifier (Type.BindingF m scope)),
    stability :: !stability
  }
  deriving (Show)

instance Traversable2 (BindingsF stability) where
  traverse2 f Bindings {terms, constructors, types, stability} =
    bindings
      <$> traverse (traverse2 f) terms
      <*> traverse (traverse2 f) constructors
      <*> traverse (traverse2 f) types
    where
      bindings terms constructors types = Bindings {terms, constructors, types, stability}

instance (Functor m) => Shift0.Functor (BindingsF stability m) where
  map = Shift.mapDefault

instance (Functor m) => Shift.Functor (BindingsF stability m) where
  map category Bindings {terms, constructors, types, stability} =
    Bindings
      { terms = fmap (Shift.map category) terms,
        constructors = fmap (Shift.map category) constructors,
        types = fmap (Shift.map category) types,
        stability
      }

instance (Semigroup stability, Applicative m) => Semigroup (BindingsF stability m scope) where
  (<>)
    Bindings {terms = terms1, constructors = constructors1, types = types1, stability = stability1}
    Bindings {terms = terms2, constructors = constructors2, types = types2, stability = stability2} =
      Bindings
        { terms = Map.unionWith (<>) terms1 terms2,
          constructors = Map.unionWith (<>) constructors1 constructors2,
          types = Map.unionWith (<>) types1 types2,
          stability = stability1 <> stability2
        }

instance (Monoid stability, Applicative m) => Monoid (BindingsF stability m scope) where
  mempty =
    Bindings
      { terms = Map.empty,
        constructors = Map.empty,
        types = Map.empty,
        stability = mempty
      }

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

updateStability stability Bindings {terms, constructors, types} =
  Bindings
    { terms,
      constructors,
      types,
      stability
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
          combine
            (position Term.:@ Identity (left@Term.Binding {selector}))
            ~(_ Term.:@ Identity Term.Binding {selector = selector'}) =
              position Term.:@ Identity left {Term.selector = selector <> selector'}
      constructors = Map.union constructors1 constructors2
      types = Map.union types1 types2

prefer :: BindingsF stable1 m scope -> BindingsF stable2 m scope -> BindingsF stable1 m scope
prefer
  Bindings {terms, constructors, types, stability}
  Bindings {terms = terms', constructors = constructors', types = types'} =
    Bindings
      { terms = terms <> terms',
        constructors = constructors <> constructors',
        types = types <> types',
        stability
      }
