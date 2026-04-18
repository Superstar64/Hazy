{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Group.Functor.Term.Declarations where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Index.Term0 as Proper.Term0
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
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
  (Int -> Proper.Term0.Index scope) ->
  Proper.Declarations scope ->
  Declarations (Term0.Index scope)
indexes index Proper.Declarations {terms, shared} =
  Declarations
    { terms = Vector.generate (length terms) (Term0.Index . index),
      shared = Vector.generate (length shared) (Term0.Share . index)
    }

(!) :: Declarations a -> Term0.Index (Scope.Declaration ':+ scope) -> a
Declarations {terms, shared} ! index = case index of
  Term0.Index (Proper.Term0.Declaration index) -> terms Vector.! index
  Term0.Share (Proper.Term0.Declaration index) -> shared Vector.! index
