module Semantic.Group.Functor.Term.Declarations where

import Data.Functor.Identity (Identity)
import Data.Vector (Vector)
import Data.Void (Void)
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Normal)
import Semantic.Locality (Local)
import Semantic.Stage (Resolve)
import {-# SOURCE #-} qualified Semantic.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { terms :: Vector a
  }

instance Functor Declarations

instance Foldable Declarations

instance Traversable Declarations

indexes ::
  (Int -> Term.Link locality) ->
  Proper.Declarations Identity Void locality' Identity Normal Resolve scope ->
  Declarations (Term.Link locality)
(!) :: Declarations a -> Term.Link Local -> a
