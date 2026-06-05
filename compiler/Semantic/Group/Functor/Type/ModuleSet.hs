module Semantic.Group.Functor.Type.ModuleSet where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Semantic.Group.Functor.Type.Declarations (Declarations)
import qualified Semantic.Group.Functor.Type.Declarations as Proper
import qualified Semantic.Index.Link.Type as Type
import Semantic.Locality (Global)

newtype ModuleSet a = ModuleSet (Vector (Declarations a))

instance Functor ModuleSet where
  fmap = fmapDefault

instance Foldable ModuleSet where
  foldMap = foldMapDefault

instance Traversable ModuleSet where
  traverse f (ModuleSet set) = ModuleSet <$> traverse (traverse f) set

(!) :: ModuleSet a -> Type.Link Global -> a
ModuleSet set ! Type.Global global local = Proper.types (set Vector.! global) Vector.! local
