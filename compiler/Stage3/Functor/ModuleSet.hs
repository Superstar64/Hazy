module Stage3.Functor.ModuleSet where

import Data.Hexafoldable (Hexafoldable (hexafoldMap))
import Data.Hexafunctor (Hexafunctor (hexamap))
import Data.Hexatraversable (Hexatraversable (..), hexafoldMapDefault, hexamapDefault)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Scope (Global)
import Stage3.Functor.Instance.Key (Key)
import Stage3.Functor.Module (Module (..))
import qualified Stage3.Functor.Module as Module

newtype ModuleSet a b c d e f = ModuleSet (Vector (Module a b c d e f))

instance Hexafunctor ModuleSet where
  hexamap = hexamapDefault

instance Hexafoldable ModuleSet where
  hexafoldMap = hexafoldMapDefault

instance Hexatraversable ModuleSet where
  hexatraverse f1 f2 f3 f4 f5 f6 (ModuleSet modules) = ModuleSet <$> traverse (hexatraverse f1 f2 f3 f4 f5 f6) modules

mapWithKey ::
  (Int -> Int -> a1 -> a2) ->
  (Int -> Int -> b1 -> b2) ->
  (Int -> Int -> c1 -> c2) ->
  (Int -> Int -> d1 -> d2) ->
  (Int -> Key Global -> e1 -> e2) ->
  (Int -> Key Global -> f1 -> f2) ->
  ModuleSet a1 b1 c1 d1 e1 f1 ->
  ModuleSet a2 b2 c2 d2 e2 f2
mapWithKey f1 f2 f3 f4 f5 f6 (ModuleSet modules) =
  ModuleSet $ Vector.imap go modules
  where
    go index = Module.mapWithKey (f1 index) (f2 index) (f3 index) (f4 index) (f5 index) (f6 index)
