module Graph.Topological7
  ( Loeb7 (..),
    Formula7 (..),
    loeb7,
    loebST7,
  )
where

import Control.Monad.ST (ST)
import Data.Heptafoldable (Heptafoldable (..))
import Data.Heptafunctor (Heptafunctor (..))
import Data.Heptatraversable (Heptatraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Septuple f a = Septuple {runSeptuple :: f a a a a a a a}

instance (Heptafunctor f) => Functor (Septuple f) where
  fmap f (Septuple septuple) = Septuple (heptamap f f f f f f f septuple)

instance (Heptafoldable f) => Foldable (Septuple f) where
  foldMap f (Septuple septuple) = heptafoldMap f f f f f f f septuple

instance (Heptatraversable f) => Traversable (Septuple f) where
  traverse f (Septuple septuple) = Septuple <$> heptatraverse f f f f f f f septuple

data Seven a b c d e f g
  = SevenA !a
  | SevenB !b
  | SevenC !c
  | SevenD !d
  | SevenE !e
  | SevenF !f
  | SevenG !g

sevenA (SevenA a) = a
sevenA _ = undefined

sevenB (SevenB a) = a
sevenB _ = undefined

sevenC (SevenC a) = a
sevenC _ = undefined

sevenD (SevenD a) = a
sevenD _ = undefined

sevenE (SevenE a) = a
sevenE _ = undefined

sevenF (SevenF a) = a
sevenF _ = undefined

sevenG (SevenG a) = a
sevenG _ = undefined

data Formula7 t s a b c d e f g z = Formula7
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s z
  }

packFormula ::
  (Heptafunctor t) =>
  t
    (Formula7 t s a b c d e f g a)
    (Formula7 t s a b c d e f g b)
    (Formula7 t s a b c d e f g c)
    (Formula7 t s a b c d e f g d)
    (Formula7 t s a b c d e f g e)
    (Formula7 t s a b c d e f g f)
    (Formula7 t s a b c d e f g g) ->
  Septuple t (Formula (Septuple t) s (Seven a b c d e f g))
packFormula =
  Septuple
    . heptamap
      (pack7 SevenA)
      (pack7 SevenB)
      (pack7 SevenC)
      (pack7 SevenD)
      (pack7 SevenE)
      (pack7 SevenF)
      (pack7 SevenG)

pack7 ::
  (Heptafunctor t) =>
  (z -> Seven a b c d e f g) ->
  Formula7 t s a b c d e f g z ->
  Formula (Septuple t) s (Seven a b c d e f g)
pack7 wrap Formula7 {cycle, run} =
  Formula
    { cycle,
      run =
        fmap wrap
          . run
          . heptamap
            (fmap sevenA)
            (fmap sevenB)
            (fmap sevenC)
            (fmap sevenD)
            (fmap sevenE)
            (fmap sevenF)
            (fmap sevenG)
          . runSeptuple
    }

newtype Loeb7 t a b c d e f g
  = Loeb7
      ( forall s.
        t
          (Formula7 t s a b c d e f g a)
          (Formula7 t s a b c d e f g b)
          (Formula7 t s a b c d e f g c)
          (Formula7 t s a b c d e f g d)
          (Formula7 t s a b c d e f g e)
          (Formula7 t s a b c d e f g f)
          (Formula7 t s a b c d e f g g)
      )

finish :: (Heptafunctor t) => Septuple t (Seven a b c d e f g) -> t a b c d e f g
finish = heptamap sevenA sevenB sevenC sevenD sevenE sevenF sevenG . runSeptuple

loeb7 ::
  (Heptatraversable t) =>
  Loeb7 t a b c d e f g ->
  t a b c d e f g
loeb7 (Loeb7 spreadsheet) = finish $ loeb $ Loeb $ packFormula spreadsheet

loebST7 ::
  (Heptatraversable t) =>
  t
    (Formula7 t s a b c d e f g a)
    (Formula7 t s a b c d e f g b)
    (Formula7 t s a b c d e f g c)
    (Formula7 t s a b c d e f g d)
    (Formula7 t s a b c d e f g e)
    (Formula7 t s a b c d e f g f)
    (Formula7 t s a b c d e f g g) ->
  ST s (t a b c d e f g)
loebST7 spreadsheet = fmap finish $ loebST $ packFormula spreadsheet
