module Graph.Topological4
  ( Loeb4 (..),
    Formula4 (..),
    loeb4,
    loebST4,
  )
where

import Control.Monad.ST (ST)
import Data.Quadrifoldable (Quadrifoldable (..))
import Data.Quadrifunctor (Quadrifunctor (..))
import Data.Quadritraversable (Quadritraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Quadruple f a = Quadruple {runQuadruple :: f a a a a}

instance (Quadrifunctor f) => Functor (Quadruple f) where
  fmap f (Quadruple quadruple) = Quadruple (quadrimap f f f f quadruple)

instance (Quadrifoldable f) => Foldable (Quadruple f) where
  foldMap f (Quadruple quadruple) = quadrifoldMap f f f f quadruple

instance (Quadritraversable f) => Traversable (Quadruple f) where
  traverse f (Quadruple quadruple) = Quadruple <$> quadritraverse f f f f quadruple

data Four a b c d
  = FourA !a
  | FourB !b
  | FourC !c
  | FourD !d

fourA (FourA a) = a
fourA _ = undefined

fourB (FourB b) = b
fourB _ = undefined

fourC (FourC c) = c
fourC _ = undefined

fourD (FourD d) = d
fourD _ = undefined

data Formula4 t s a b c d z = Formula4
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s z
  }

packFormula ::
  (Quadrifunctor t) =>
  t (Formula4 t s a b c d a) (Formula4 t s a b c d b) (Formula4 t s a b c d c) (Formula4 t s a b c d d) ->
  Quadruple t (Formula (Quadruple t) s (Four a b c d))
packFormula = Quadruple . quadrimap (pack FourA) (pack FourB) (pack FourC) (pack FourD)

pack :: (Quadrifunctor t) => (z -> Four a b c d) -> Formula4 t s a b c d z -> Formula (Quadruple t) s (Four a b c d)
pack wrap Formula4 {cycle, run} =
  Formula
    { cycle,
      run = fmap wrap . run . quadrimap (fmap fourA) (fmap fourB) (fmap fourC) (fmap fourD) . runQuadruple
    }

newtype Loeb4 t a b c d
  = Loeb4
      ( forall s.
        t
          (Formula4 t s a b c d a)
          (Formula4 t s a b c d b)
          (Formula4 t s a b c d c)
          (Formula4 t s a b c d d)
      )

finish ::
  (Quadrifunctor t) =>
  Quadruple t (Four a b c d) ->
  t a b c d
finish = quadrimap fourA fourB fourC fourD . runQuadruple

loeb4 ::
  (Quadritraversable t) =>
  Loeb4 t a b c d ->
  t a b c d
loeb4 (Loeb4 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST4 ::
  (Quadritraversable t) =>
  t
    (Formula4 t s a b c d a)
    (Formula4 t s a b c d b)
    (Formula4 t s a b c d c)
    (Formula4 t s a b c d d) ->
  ST s (t a b c d)
loebST4 spreadsheet = fmap finish $ loebST $ packFormula spreadsheet
