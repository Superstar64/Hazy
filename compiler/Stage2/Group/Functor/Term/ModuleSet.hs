module Stage2.Group.Functor.Term.ModuleSet where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Group.Functor.Term.Declarations (Declarations)
import qualified Stage2.Group.Functor.Term.Declarations as Declarations
import qualified Stage2.Index.Link.Term as Proper.Term
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Locality as Locality

newtype ModuleSet a = ModuleSet (Vector (Declarations a))

instance Functor ModuleSet where
  fmap = fmapDefault

instance Foldable ModuleSet where
  foldMap = foldMapDefault

instance Traversable ModuleSet where
  traverse f (ModuleSet set) = ModuleSet <$> traverse (traverse f) set

(!) :: ModuleSet a -> Term.Link Locality.Global -> a
ModuleSet vector ! index = case index of
  Proper.Term.Global global local ->
    Declarations.terms (vector Vector.! global) Vector.! local
