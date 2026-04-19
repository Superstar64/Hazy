module Stage2.Group.Functor.Term.ModuleSet where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Group.Functor.Term.Declarations (Declarations)
import qualified Stage2.Group.Functor.Term.Declarations as Declarations
import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Index.Term0 as Proper.Term0
import Stage2.Scope (Global)

newtype ModuleSet a = ModuleSet (Vector (Declarations a))

instance Functor ModuleSet where
  fmap = fmapDefault

instance Foldable ModuleSet where
  foldMap = foldMapDefault

instance Traversable ModuleSet where
  traverse f (ModuleSet set) = ModuleSet <$> traverse (traverse f) set

(!) :: ModuleSet a -> Term0.Index Global -> a
ModuleSet vector ! index = case index of
  Term0.Index (Proper.Term0.Global global local) ->
    Declarations.terms (vector Vector.! global) Vector.! local
  Term0.Share (Proper.Term0.Global global local) ->
    Declarations.shared (vector Vector.! global) Vector.! local
