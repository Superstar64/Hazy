module Stage3.Functor.ModuleSet where

import Data.Octafoldable (Octafoldable (octafoldMap))
import Data.Octafunctor (Octafunctor (octamap))
import Data.Octatraversable (Octatraversable (..), octafoldMapDefault, octamapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Scope (Global)
import Stage3.Functor.Instance.Key (Key)
import Stage3.Functor.Module (Module (..))
import qualified Stage3.Functor.Module as Module

newtype ModuleSet a b c d e f g h = ModuleSet (Vector (Module a b c d e f g h))

instance Octafunctor ModuleSet where
  octamap = octamapDefault

instance Octafoldable ModuleSet where
  octafoldMap = octafoldMapDefault

instance Octatraversable ModuleSet where
  octatraverse f1 f2 f3 f4 f5 f6 f7 f8 (ModuleSet modules) =
    ModuleSet <$> traverse (octatraverse f1 f2 f3 f4 f5 f6 f7 f8) modules

mapWithKey ::
  (Int -> Int -> a1 -> a2) ->
  (Int -> Int -> b1 -> b2) ->
  (Int -> Int -> c1 -> c2) ->
  (Int -> Int -> d1 -> d2) ->
  (Int -> Int -> e1 -> e2) ->
  (Int -> Int -> f1 -> f2) ->
  (Int -> Key Global -> g1 -> g2) ->
  (Int -> Key Global -> h1 -> h2) ->
  ModuleSet a1 b1 c1 d1 e1 f1 g1 h1 ->
  ModuleSet a2 b2 c2 d2 e2 f2 g2 h2
mapWithKey f1 f2 f3 f4 f5 f6 f7 f8 (ModuleSet modules) =
  ModuleSet $ Vector.imap go modules
  where
    go index = Module.mapWithKey (f1 index) (f2 index) (f3 index) (f4 index) (f5 index) (f6 index) (f7 index) (f8 index)
