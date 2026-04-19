module Stage2.Group.Functor.Type.ModuleSet where

import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Group.Functor.Type.Declarations (Declarations)
import qualified Stage2.Group.Functor.Type.Declarations as Proper
import qualified Stage2.Index.Type0 as Type0
import Stage2.Scope (Global)

newtype ModuleSet a = ModuleSet (Vector (Declarations a))

instance Functor ModuleSet where
  fmap = fmapDefault

instance Foldable ModuleSet where
  foldMap = foldMapDefault

instance Traversable ModuleSet where
  traverse f (ModuleSet set) = ModuleSet <$> traverse (traverse f) set

(!) :: ModuleSet a -> Type0.Index Global -> a
ModuleSet set ! Type0.Global global local = Proper.types (set Vector.! global) Vector.! local
