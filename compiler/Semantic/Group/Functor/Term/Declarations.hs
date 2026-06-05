{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Group.Functor.Term.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Semantic.Index.Link.Term as Term
import Semantic.Layout (Normal)
import Semantic.Locality (Local)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declarations as Proper

newtype Declarations a = Declarations
  { terms :: Vector a
  }

instance Functor Declarations where
  fmap = fmapDefault

instance Foldable Declarations where
  foldMap = foldMapDefault

instance Traversable Declarations where
  traverse f Declarations {terms} = declarations <$> traverse f terms
    where
      declarations terms = Declarations {terms}

indexes ::
  (Int -> Term.Link locality) ->
  Proper.Declarations locality' Normal Resolve scope ->
  Declarations (Term.Link locality)
indexes index Proper.Declarations {terms} =
  Declarations
    { terms = Vector.generate (length terms) index
    }

(!) :: Declarations a -> Term.Link Local -> a
Declarations {terms} ! index = case index of
  Term.Declaration index -> terms Vector.! index
