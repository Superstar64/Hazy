module Graph.Topological5
  ( Loeb5 (..),
    Formula5 (..),
    loeb5,
    loebST5,
  )
where

import Control.Monad.ST (ST)
import Data.Pentafoldable (Pentafoldable (..))
import Data.Pentafunctor (Pentafunctor (..))
import Data.Pentatraversable (Pentatraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Quinetuple f a = Quinetuple {runQuinetuple :: f a a a a a}

instance (Pentafunctor f) => Functor (Quinetuple f) where
  fmap f (Quinetuple penta) = Quinetuple (pentamap f f f f f penta)

instance (Pentafoldable f) => Foldable (Quinetuple f) where
  foldMap f (Quinetuple penta) = pentafoldMap f f f f f penta

instance (Pentatraversable f) => Traversable (Quinetuple f) where
  traverse f (Quinetuple penta) = Quinetuple <$> pentatraverse f f f f f penta

data Five a b c d e
  = FiveA !a
  | FiveB !b
  | FiveC !c
  | FiveD !d
  | FiveE !e

fiveA (FiveA a) = a
fiveA _ = undefined

fiveB (FiveB b) = b
fiveB _ = undefined

fiveC (FiveC c) = c
fiveC _ = undefined

fiveD (FiveD d) = d
fiveD _ = undefined

fiveE (FiveE e) = e
fiveE _ = undefined

data Formula5 t s a b c d e z = Formula5
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s z
  }

packFormula ::
  (Pentafunctor t) =>
  t
    (Formula5 t s a b c d e a)
    (Formula5 t s a b c d e b)
    (Formula5 t s a b c d e c)
    (Formula5 t s a b c d e d)
    (Formula5 t s a b c d e e) ->
  Quinetuple t (Formula (Quinetuple t) s (Five a b c d e))
packFormula =
  Quinetuple
    . pentamap
      (pack FiveA)
      (pack FiveB)
      (pack FiveC)
      (pack FiveD)
      (pack FiveE)

pack ::
  (Pentafunctor t) =>
  (z -> Five a b c d e) ->
  Formula5 t s a b c d e z ->
  Formula (Quinetuple t) s (Five a b c d e)
pack wrap Formula5 {cycle, run} =
  Formula
    { cycle,
      run =
        fmap wrap
          . run
          . pentamap
            (fmap fiveA)
            (fmap fiveB)
            (fmap fiveC)
            (fmap fiveD)
            (fmap fiveE)
          . runQuinetuple
    }

newtype Loeb5 t a b c d e
  = Loeb5
      ( forall s.
        t
          (Formula5 t s a b c d e a)
          (Formula5 t s a b c d e b)
          (Formula5 t s a b c d e c)
          (Formula5 t s a b c d e d)
          (Formula5 t s a b c d e e)
      )

finish ::
  (Pentafunctor t) =>
  Quinetuple t (Five a b c d e) ->
  t a b c d e
finish = pentamap fiveA fiveB fiveC fiveD fiveE . runQuinetuple

loeb5 ::
  (Pentatraversable t) =>
  Loeb5 t a b c d e ->
  t a b c d e
loeb5 (Loeb5 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST5 ::
  (Pentatraversable t) =>
  t
    (Formula5 t s a b c d e a)
    (Formula5 t s a b c d e b)
    (Formula5 t s a b c d e c)
    (Formula5 t s a b c d e d)
    (Formula5 t s a b c d e e) ->
  ST s (t a b c d e)
loebST5 spreadsheet = fmap finish $ loebST $ packFormula spreadsheet
