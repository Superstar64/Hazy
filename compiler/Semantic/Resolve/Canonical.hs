module Semantic.Resolve.Canonical where

import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import Error (moduleNotFound)
import Semantic.Resolve.Bindings (BindingsF)
import Semantic.Resolve.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Syntax.Tree.Marked (Marked (..))
import Syntax.Variable (FullQualifiers)

type Canonical = CanonicalF Identity

newtype CanonicalF loeb scope = Canonical {runCanonical :: Map FullQualifiers (BindingsF () loeb scope)}
  deriving (Show)

instance Traversable2 CanonicalF where
  traverse2 f (Canonical canonical) = Canonical <$> traverse (traverse2 f) canonical

instance (Functor loeb) => Shift0.Functor (CanonicalF loeb) where
  map = Shift.mapDefault

instance (Functor loeb) => Shift.Functor (CanonicalF loeb) where
  map category (Canonical canonical) = Canonical (fmap (Shift.map category) canonical)

infixl 3 !

Canonical table ! position :@ name = case Map.lookup name table of
  Just bindings -> bindings
  Nothing -> moduleNotFound position

empty = Canonical Map.empty
