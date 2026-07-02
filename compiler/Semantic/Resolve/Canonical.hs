module Semantic.Resolve.Canonical where

import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import Error (moduleNotFound)
import Semantic.Resolve.Bindings (BindingsF)
import Semantic.Resolve.Functor2 (Traversable2 (..))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (FullQualifiers)

type Canonical = CanonicalF Identity

newtype CanonicalF m scope = Canonical {runCanonical :: Map FullQualifiers (BindingsF () m scope)}
  deriving (Show)

instance Traversable2 CanonicalF where
  traverse2 f (Canonical canonical) = Canonical <$> traverse (traverse2 f) canonical

instance (Functor m) => Shift (CanonicalF m) where
  shift = shiftDefault

instance (Functor m) => Shift.Functor (CanonicalF m) where
  map category (Canonical canonical) = Canonical (fmap (Shift.map category) canonical)

infixl 3 !

Canonical table ! position :@ name = case Map.lookup name table of
  Just bindings -> bindings
  Nothing -> moduleNotFound position

empty = Canonical Map.empty
