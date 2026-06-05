module Semantic.Resolve.Functor.Binding.Term where

import Data.Traversable (fmapDefault, foldMapDefault)
import Error (duplicateVariableEntries)
import Semantic.Resolve.Functor.Same (Same (same))
import Syntax.Position (Position)

data Binding a = Binding
  { position :: !Position,
    value :: a
  }
  deriving (Show)

instance (Same a) => Semigroup (Binding a) where
  left <> right =
    Binding
      { position = position left,
        value = same abort (value left) (value right)
      }
    where
      abort = duplicateVariableEntries [position left, position right]

instance Functor Binding where
  fmap = fmapDefault

instance Foldable Binding where
  foldMap = foldMapDefault

instance Traversable Binding where
  traverse f Binding {position, value} = Binding position <$> f value
