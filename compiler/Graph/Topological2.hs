module Graph.Topological2
  ( Loeb2 (..),
    Formula2 (..),
    loeb2,
    loebST2,
  )
where

import Control.Monad.ST (ST)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Double t a = Double {runDouble :: t a a}

instance (Bifunctor t) => Functor (Double t) where
  fmap f (Double double) = Double (bimap f f double)

instance (Bifoldable t) => Foldable (Double t) where
  foldMap f (Double double) = bifoldMap f f double

instance (Bitraversable t) => Traversable (Double t) where
  traverse f (Double double) = Double <$> bitraverse f f double

data Two a b
  = TwoA !a
  | TwoB !b

twoA (TwoA a) = a
twoA _ = undefined

twoB (TwoB b) = b
twoB _ = undefined

data Formula2 t s a b z = Formula2
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) -> ST s z
  }

packFormula ::
  (Bifunctor t) =>
  t (Formula2 t s a b a) (Formula2 t s a b b) ->
  Double t (Formula (Double t) s (Two a b))
packFormula = Double . bimap (pack TwoA) (pack TwoB)

pack :: (Bifunctor f) => (z -> Two a b) -> Formula2 f s a b z -> Formula (Double f) s (Two a b)
pack wrap Formula2 {cycle, run} =
  Formula
    { cycle,
      run = fmap wrap . run . bimap (fmap twoA) (fmap twoB) . runDouble
    }

newtype Loeb2 t a b
  = Loeb2
      ( forall s.
        t
          (Formula2 t s a b a)
          (Formula2 t s a b b)
      )

finish :: (Bifunctor t) => Double t (Two a b) -> t a b
finish = bimap twoA twoB . runDouble

loeb2 ::
  (Bitraversable t) =>
  Loeb2 t a b ->
  t a b
loeb2 (Loeb2 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST2 ::
  (Bitraversable t) =>
  t
    (Formula2 t s a b a)
    (Formula2 t s a b b) ->
  ST s (t a b)
loebST2 = fmap finish . loebST . packFormula
