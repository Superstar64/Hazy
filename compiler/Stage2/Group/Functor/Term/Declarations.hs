{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Group.Functor.Term.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Group.Index.Link.Term as Term
import qualified Stage2.Index.Link.Term as Proper.Term
import Stage2.Locality (Local)
import qualified Stage2.Tree.Declarations as Proper

data Declarations a = Declarations
  { terms :: !(Vector a),
    shared :: !(Vector a)
  }

instance Functor Declarations where
  fmap = fmapDefault

instance Foldable Declarations where
  foldMap = foldMapDefault

instance Traversable Declarations where
  traverse f Declarations {terms, shared} = declarations <$> traverse f terms <*> traverse f shared
    where
      declarations terms shared = Declarations {terms, shared}

indexes ::
  (Int -> Proper.Term.Link locality) ->
  Proper.Declarations locality' scope ->
  Declarations (Term.Link locality)
indexes index Proper.Declarations {terms, shared} =
  Declarations
    { terms = Vector.generate (length terms) (Term.Link . index),
      shared = Vector.generate (length shared) (Term.Share . index)
    }

(!) :: Declarations a -> Term.Link Local -> a
Declarations {terms, shared} ! index = case index of
  Term.Link (Proper.Term.Declaration index) -> terms Vector.! index
  Term.Share (Proper.Term.Declaration index) -> shared Vector.! index
