module Graph.Topological3
  ( Loeb3 (..),
    Formula3 (..),
    loeb3,
    loebST3,
  )
where

import Control.Monad.ST (ST)
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Triple f a = Triple {runTriple :: f a a a}

instance (Trifunctor f) => Functor (Triple f) where
  fmap f (Triple triple) = Triple (trimap f f f triple)

instance (Trifoldable f) => Foldable (Triple f) where
  foldMap f (Triple triple) = trifoldMap f f f triple

instance (Tritraversable f) => Traversable (Triple f) where
  traverse f (Triple triple) = Triple <$> tritraverse f f f triple

data Three a b c
  = ThreeA !a
  | ThreeB !b
  | ThreeC !c

threeA (ThreeA a) = a
threeA _ = undefined

threeB (ThreeB b) = b
threeB _ = undefined

threeC (ThreeC c) = c
threeC _ = undefined

data Formula3 t s a b c z = Formula3
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) -> ST s z
  }

packFormula ::
  (Trifunctor t) =>
  t (Formula3 t s a b c a) (Formula3 t s a b c b) (Formula3 t s a b c c) ->
  Triple t (Formula (Triple t) s (Three a b c))
packFormula = Triple . trimap (pack ThreeA) (pack ThreeB) (pack ThreeC)

pack :: (Trifunctor t) => (z -> Three a b c) -> Formula3 t s a b c z -> Formula (Triple t) s (Three a b c)
pack wrap Formula3 {cycle, run} =
  Formula
    { cycle,
      run = fmap wrap . run . trimap (fmap threeA) (fmap threeB) (fmap threeC) . runTriple
    }

newtype Loeb3 t a b c
  = Loeb3
      ( forall s.
        t
          (Formula3 t s a b c a)
          (Formula3 t s a b c b)
          (Formula3 t s a b c c)
      )

finish :: (Trifunctor t) => Triple t (Three a b c) -> t a b c
finish = trimap threeA threeB threeC . runTriple

loeb3 ::
  (Tritraversable t) =>
  Loeb3 t a b c ->
  t a b c
loeb3 (Loeb3 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST3 ::
  (Tritraversable t) =>
  t
    (Formula3 t s a b c a)
    (Formula3 t s a b c b)
    (Formula3 t s a b c c) ->
  ST s (t a b c)
loebST3 spreadsheet = fmap finish $ loebST $ packFormula spreadsheet
