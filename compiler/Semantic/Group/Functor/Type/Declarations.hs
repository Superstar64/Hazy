{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Group.Functor.Type.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Semantic.Index.Link.Type as Type
import Semantic.Layout (Normal)
import Semantic.Locality (Local)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declarations as Proper

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
  Proper.Declarations locality' Normal Resolve scope ->
  Declarations (Type.Link locality)
indexes index Proper.Declarations {types} =
  Declarations
    { types = Vector.generate (length types) index
    }

(!) :: Declarations a -> Type.Link Local -> a
Declarations {types} ! Type.Declaration index = types Vector.! index
