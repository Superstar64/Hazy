module Graph.Topological6
  ( Loeb6 (..),
    Formula6 (..),
    loeb6,
    loebST6,
  )
where

import Control.Monad.ST (ST)
import Data.Hexafoldable (Hexafoldable (..))
import Data.Hexafunctor (Hexafunctor (..))
import Data.Hexatraversable (Hexatraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Sixtuple f a = Sixtuple {runSixtuple :: f a a a a a a}

instance (Hexafunctor f) => Functor (Sixtuple f) where
  fmap f (Sixtuple sixtuple) = Sixtuple (hexamap f f f f f f sixtuple)

instance (Hexafoldable f) => Foldable (Sixtuple f) where
  foldMap f (Sixtuple sixtuple) = hexafoldMap f f f f f f sixtuple

instance (Hexatraversable f) => Traversable (Sixtuple f) where
  traverse f (Sixtuple sixtuple) = Sixtuple <$> hexatraverse f f f f f f sixtuple

data Six a b c d e f
  = SixA !a
  | SixB !b
  | SixC !c
  | SixD !d
  | SixE !e
  | SixF !f

sixA (SixA a) = a
sixA _ = undefined

sixB (SixB a) = a
sixB _ = undefined

sixC (SixC a) = a
sixC _ = undefined

sixD (SixD a) = a
sixD _ = undefined

sixE (SixE a) = a
sixE _ = undefined

sixF (SixF a) = a
sixF _ = undefined

data Formula6 t s a b c d e f z = Formula6
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s z
  }

packFormula ::
  (Hexafunctor t) =>
  t
    (Formula6 t s a b c d e f a)
    (Formula6 t s a b c d e f b)
    (Formula6 t s a b c d e f c)
    (Formula6 t s a b c d e f d)
    (Formula6 t s a b c d e f e)
    (Formula6 t s a b c d e f f) ->
  Sixtuple t (Formula (Sixtuple t) s (Six a b c d e f))
packFormula =
  Sixtuple
    . hexamap
      (pack SixA)
      (pack SixB)
      (pack SixC)
      (pack SixD)
      (pack SixE)
      (pack SixF)

pack ::
  (Hexafunctor t) =>
  (z -> Six a b c d e f) ->
  Formula6 t s a b c d e f z ->
  Formula (Sixtuple t) s (Six a b c d e f)
pack wrap Formula6 {cycle, run} =
  Formula
    { cycle,
      run =
        fmap wrap
          . run
          . hexamap
            (fmap sixA)
            (fmap sixB)
            (fmap sixC)
            (fmap sixD)
            (fmap sixE)
            (fmap sixF)
          . runSixtuple
    }

newtype Loeb6 t a b c d e f
  = Loeb6
      ( forall s.
        t
          (Formula6 t s a b c d e f a)
          (Formula6 t s a b c d e f b)
          (Formula6 t s a b c d e f c)
          (Formula6 t s a b c d e f d)
          (Formula6 t s a b c d e f e)
          (Formula6 t s a b c d e f f)
      )

finish ::
  (Hexafunctor t) =>
  Sixtuple t (Six a b c d e f) ->
  t a b c d e f
finish = hexamap sixA sixB sixC sixD sixE sixF . runSixtuple

loeb6 ::
  (Hexatraversable t) =>
  Loeb6 t a b c d e f ->
  t a b c d e f
loeb6 (Loeb6 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST6 ::
  (Hexatraversable t) =>
  t
    (Formula6 t s a b c d e f a)
    (Formula6 t s a b c d e f b)
    (Formula6 t s a b c d e f c)
    (Formula6 t s a b c d e f d)
    (Formula6 t s a b c d e f e)
    (Formula6 t s a b c d e f f) ->
  ST s (t a b c d e f)
loebST6 spreadsheet = fmap finish $ loebST $ packFormula spreadsheet
