module Semantic.Group.Functor.Type.Declarations where

import Data.Vector (Vector)
import qualified Semantic.Index.Link.Type as Type
import Semantic.Layout (Normal)
import Semantic.Locality (Local)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} qualified Semantic.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { types :: Vector a
  }

instance Functor Declarations

instance Foldable Declarations

instance Traversable Declarations

indexes ::
  (Int -> Type.Link locality) ->
  Proper.Declarations locality' Normal Resolve scope ->
  Declarations (Type.Link locality)
(!) :: Declarations a -> Type.Link Local -> a
