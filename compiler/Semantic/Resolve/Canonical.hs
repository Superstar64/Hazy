module Semantic.Resolve.Canonical where

import Data.Map (Map)
import qualified Data.Map as Map
import Error (moduleNotFound)
import Semantic.Resolve.Bindings (Bindings)
import qualified Semantic.Resolve.Bindings as Bindings
import qualified Semantic.Resolve.Detail.Binding.Constructor as Constructor
import qualified Semantic.Resolve.Detail.Binding.Term as Term
import qualified Semantic.Resolve.Detail.Binding.Type as Type
import qualified Semantic.Resolve.Functor.Canonical as Functor
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (FullQualifiers)

newtype Canonical scope = Canonical {runCanonical :: Map FullQualifiers (Bindings () scope)}
  deriving (Show)

instance Shift Canonical where
  shift = shiftDefault

instance Shift.Functor Canonical where
  map category (Canonical canonical) = Canonical (fmap (Shift.map category) canonical)

infixl 3 !

Canonical table ! position :@ name = case Map.lookup name table of
  Just bindings -> bindings
  Nothing -> moduleNotFound position

empty = Canonical Map.empty

fromFunctor ::
  Functor.Canonical
    (Term.Binding scope)
    (Constructor.Binding scope)
    (Type.Binding scope) ->
  Canonical scope
fromFunctor (Functor.Canonical canoncial) = Canonical (fmap Bindings.fromFunctor canoncial)

toFunctor ::
  Canonical scope ->
  Functor.Canonical
    (Term.Binding scope)
    (Constructor.Binding scope)
    (Type.Binding scope)
toFunctor (Canonical canoncial) = Functor.Canonical (fmap Bindings.toFunctor canoncial)
