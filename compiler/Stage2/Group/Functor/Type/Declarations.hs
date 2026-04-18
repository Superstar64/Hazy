{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Group.Functor.Type.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Index.Type0 as Type0
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { types :: Vector a
  }

instance Functor Declarations where
  fmap = fmapDefault

instance Foldable Declarations where
  foldMap = foldMapDefault

instance Traversable Declarations where
  traverse f Declarations {types} = declarations <$> traverse f types
    where
      declarations types = Declarations {types}

indexes ::
  (Int -> Type0.Index scope) ->
  Proper.Declarations scope ->
  Declarations (Type0.Index scope)
indexes index Proper.Declarations {types} =
  Declarations
    { types = Vector.generate (length types) index
    }

(!) :: Declarations a -> Type0.Index (Scope.Declaration ':+ scope) -> a
Declarations {types} ! index = case index of
  Type0.Declaration index -> types Vector.! index
