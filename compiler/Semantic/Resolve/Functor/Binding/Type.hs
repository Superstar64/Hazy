module Semantic.Resolve.Functor.Binding.Type where

import Data.Set (Set)
import Data.Traversable (fmapDefault, foldMapDefault)
import Error (duplicateTypeEntries)
import Semantic.Resolve.Functor.Same (Same (..))
import Syntax.Position (Position)
import Syntax.Variable (Constructor, Variable)

data Binding a = Binding
  { position :: !Position,
    value :: a,
    constructors :: !(Set Constructor),
    fields :: !(Set Variable)
  }
  deriving (Show)

instance (Same a) => Semigroup (Binding a) where
  left <> right =
    left
      { position = position left,
        value = same abort (value left) (value right)
      }
    where
      abort = duplicateTypeEntries [position left, position right]

instance Functor Binding where
  fmap = fmapDefault

instance Foldable Binding where
  foldMap = foldMapDefault

instance Traversable Binding where
  traverse f Binding {position, value, constructors, fields} =
    Binding position <$> f value <*> pure constructors <*> pure fields
