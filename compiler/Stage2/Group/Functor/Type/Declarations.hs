{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Group.Functor.Type.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Index.Link.Type as Type
import Stage2.Locality (Local)
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
  (Int -> Type.Link locality) ->
  Proper.Declarations locality' scope ->
  Declarations (Type.Link locality)
indexes index Proper.Declarations {types} =
  Declarations
    { types = Vector.generate (length types) index
    }

(!) :: Declarations a -> Type.Link Local -> a
Declarations {types} ! Type.Declaration index = types Vector.! index
