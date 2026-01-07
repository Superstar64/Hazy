module Stage2.Resolve.Canonical where

import Data.Map (Map)
import qualified Data.Map as Map
import Error (moduleNotFound)
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (FullQualifiers)
import Stage2.Resolve.Bindings (Bindings)
import qualified Stage2.Resolve.Bindings as Bindings
import qualified Stage2.Resolve.Detail.Binding.Constructor as Constructor
import qualified Stage2.Resolve.Detail.Binding.Term as Term
import qualified Stage2.Resolve.Detail.Binding.Type as Type
import qualified Stage2.Resolve.Functor.Canonical as Functor
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

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
