module Stage2.Group.Functor.Type.Declarations where

import Data.Vector (Vector)
import qualified Stage2.Index.Link.Type as Type
import Stage2.Layout (Normal)
import Stage2.Locality (Local)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { types :: Vector a
  }

instance Functor Declarations

instance Foldable Declarations

instance Traversable Declarations

indexes ::
  (Int -> Type.Link locality) ->
  Proper.Declarations locality' Normal scope ->
  Declarations (Type.Link locality)
(!) :: Declarations a -> Type.Link Local -> a
