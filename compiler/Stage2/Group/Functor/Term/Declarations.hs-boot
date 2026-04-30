module Stage2.Group.Functor.Term.Declarations where

import Data.Vector (Vector)
import qualified Stage2.Group.Index.Link.Term as Term
import qualified Stage2.Index.Link.Term as Proper.Term
import Stage2.Layout (Normal)
import Stage2.Locality (Local)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Proper

data Declarations a = Declarations
  { terms :: !(Vector a),
    shared :: !(Vector a)
  }

instance Functor Declarations

instance Foldable Declarations

instance Traversable Declarations

indexes ::
  (Int -> Proper.Term.Link locality) ->
  Proper.Declarations locality' Normal scope ->
  Declarations (Term.Link locality)
(!) :: Declarations a -> Term.Link Local -> a
