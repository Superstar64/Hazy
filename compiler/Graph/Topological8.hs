module Graph.Topological8
  ( Loeb8 (..),
    Formula8 (..),
    loeb8,
    loebST8,
  )
where

import Control.Monad.ST (ST)
import Data.Octafoldable (Octafoldable (..))
import Data.Octafunctor (Octafunctor (..))
import Data.Octatraversable (Octatraversable (..))
import Graph.Topological1 (Formula (..), Loeb (..), loeb, loebST)
import Prelude hiding (Double, map)

newtype Octuple f a = Octuple {runOctuple :: f a a a a a a a a}

instance (Octafunctor f) => Functor (Octuple f) where
  fmap f (Octuple oct) = Octuple (octamap f f f f f f f f oct)

instance (Octafoldable f) => Foldable (Octuple f) where
  foldMap f (Octuple oct) = octafoldMap f f f f f f f f oct

instance (Octatraversable f) => Traversable (Octuple f) where
  traverse f (Octuple oct) = Octuple <$> octatraverse f f f f f f f f oct

data Eight a b c d e f g h
  = EightA !a
  | EightB !b
  | EightC !c
  | EightD !d
  | EightE !e
  | EightF !f
  | EightG !g
  | EightH !h

eightA (EightA a) = a
eightA _ = undefined

eightB (EightB a) = a
eightB _ = undefined

eightC (EightC a) = a
eightC _ = undefined

eightD (EightD a) = a
eightD _ = undefined

eightE (EightE a) = a
eightE _ = undefined

eightF (EightF a) = a
eightF _ = undefined

eightG (EightG a) = a
eightG _ = undefined

eightH (EightH a) = a
eightH _ = undefined

data Formula8 t s a b c d e f g h z = Formula8
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s z
  }

packFormula8 ::
  (Octafunctor t) =>
  t
    (Formula8 t s a b c d e f g h a)
    (Formula8 t s a b c d e f g h b)
    (Formula8 t s a b c d e f g h c)
    (Formula8 t s a b c d e f g h d)
    (Formula8 t s a b c d e f g h e)
    (Formula8 t s a b c d e f g h f)
    (Formula8 t s a b c d e f g h g)
    (Formula8 t s a b c d e f g h h) ->
  Octuple t (Formula (Octuple t) s (Eight a b c d e f g h))
packFormula8 =
  Octuple
    . octamap
      (pack8 EightA)
      (pack8 EightB)
      (pack8 EightC)
      (pack8 EightD)
      (pack8 EightE)
      (pack8 EightF)
      (pack8 EightG)
      (pack8 EightH)

pack8 ::
  (Octafunctor t) =>
  (z -> Eight a b c d e f g h) ->
  Formula8 t s a b c d e f g h z ->
  Formula (Octuple t) s (Eight a b c d e f g h)
pack8 wrap Formula8 {cycle, run} =
  Formula
    { cycle,
      run =
        fmap wrap
          . run
          . octamap
            (fmap eightA)
            (fmap eightB)
            (fmap eightC)
            (fmap eightD)
            (fmap eightE)
            (fmap eightF)
            (fmap eightG)
            (fmap eightH)
          . runOctuple
    }

newtype Loeb8 t a b c d e f g h
  = Loeb8
      ( forall s.
        t
          (Formula8 t s a b c d e f g h a)
          (Formula8 t s a b c d e f g h b)
          (Formula8 t s a b c d e f g h c)
          (Formula8 t s a b c d e f g h d)
          (Formula8 t s a b c d e f g h e)
          (Formula8 t s a b c d e f g h f)
          (Formula8 t s a b c d e f g h g)
          (Formula8 t s a b c d e f g h h)
      )

finish :: (Octafunctor t) => Octuple t (Eight a b c d e f g h) -> t a b c d e f g h
finish = octamap eightA eightB eightC eightD eightE eightF eightG eightH . runOctuple

loeb8 :: (Octatraversable t) => Loeb8 t a b c d e f g h -> t a b c d e f g h
loeb8 (Loeb8 spreadsheet) = finish $ loeb $ Loeb $ packFormula8 spreadsheet

loebST8 ::
  (Octatraversable t) =>
  t
    (Formula8 t s a b c d e f g h a)
    (Formula8 t s a b c d e f g h b)
    (Formula8 t s a b c d e f g h c)
    (Formula8 t s a b c d e f g h d)
    (Formula8 t s a b c d e f g h e)
    (Formula8 t s a b c d e f g h f)
    (Formula8 t s a b c d e f g h g)
    (Formula8 t s a b c d e f g h h) ->
  ST s (t a b c d e f g h)
loebST8 spreadsheet = fmap finish $ loebST $ packFormula8 spreadsheet
