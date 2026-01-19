module Data.Acyclic
  ( Loeb (..),
    loeb,
    loebST,
    Loeb2 (..),
    loeb2,
    loebST2,
    Loeb3 (..),
    loeb3,
    loebST3,
    Loeb4 (..),
    loeb4,
    loebST4,
    Loeb5 (..),
    loeb5,
    loebST5,
    Loeb6 (..),
    loeb6,
    loebST6,
    Loeb7 (..),
    loeb7,
    loebST7,
  )
where

import Control.Monad.ST (ST)
import Data.Acyclic.Axiom (Loeb (..), loeb, loebST)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Heptafoldable (Heptafoldable (..))
import Data.Heptafunctor (Heptafunctor (..))
import Data.Heptatraversable (Heptatraversable (..))
import Data.Hexafoldable (Hexafoldable (..))
import Data.Hexafunctor (Hexafunctor (..))
import Data.Hexatraversable (Hexatraversable (..))
import Data.Pentafoldable (Pentafoldable (..))
import Data.Pentafunctor (Pentafunctor (..))
import Data.Pentatraversable (Pentatraversable (..))
import Data.Quadrifoldable (Quadrifoldable (..))
import Data.Quadrifunctor (Quadrifunctor (..))
import Data.Quadritraversable (Quadritraversable (..))
import Data.Trifoldable (Trifoldable (..))
import Data.Trifunctor (Trifunctor (..))
import Data.Tritraversable (Tritraversable (..))
import Data.Void (Void)
import Prelude hiding (Double, map)

newtype Double f a = Double {runDouble :: f a a}

instance (Bifunctor f) => Functor (Double f) where
  fmap f (Double double) = Double (bimap f f double)

instance (Bifoldable f) => Foldable (Double f) where
  foldMap f (Double double) = bifoldMap f f double

instance (Bitraversable f) => Traversable (Double f) where
  traverse f (Double double) = Double <$> bitraverse f f double

data Two a b
  = TwoA !a
  | TwoB !b

twoA (TwoA a) = a
twoA _ = undefined

twoB (TwoB b) = b
twoB _ = undefined

packDoubleM :: (Bifunctor t, Functor f) => t (f a) (f b) -> Double t (f (Two a b))
packDoubleM = Double . bimap (fmap TwoA) (fmap TwoB)

unpackDouble :: (Bifunctor t) => Double t (Two a b) -> t a b
unpackDouble = bimap twoA twoB . runDouble

unpackDoubleM :: (Bifunctor f, Functor t) => Double f (t (Two a b)) -> f (t a) (t b)
unpackDoubleM = bimap (fmap twoA) (fmap twoB) . runDouble

goDouble cell = case cell of
  TwoA run -> fmap TwoA . run . unpackDoubleM
  TwoB run -> fmap TwoB . run . unpackDoubleM

newtype Loeb2 f s a b
  = Loeb2
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) -> ST s a)
          (Void, f (ST s a) (ST s b) -> ST s b)
      )

loeb2 ::
  (Bitraversable f) =>
  Loeb2 f s a b ->
  f a b
loeb2 (Loeb2 spreadsheet) = unpackDouble $ loeb $ Loeb $ fmap goDouble <$> packDoubleM spreadsheet

loebST2 ::
  (Bitraversable t) =>
  t
    (Void, t (ST s a) (ST s b) -> ST s a)
    (Void, t (ST s a) (ST s b) -> ST s b) ->
  ST s (t a b)
loebST2 spreadsheet = fmap unpackDouble $ loebST $ fmap goDouble <$> packDoubleM spreadsheet

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

packTripleM :: (Trifunctor f, Functor m) => f (m a) (m b) (m c) -> Triple f (m (Three a b c))
packTripleM = Triple . trimap (fmap ThreeA) (fmap ThreeB) (fmap ThreeC)

unpackTriple :: (Trifunctor t) => Triple t (Three a b c) -> t a b c
unpackTriple = trimap threeA threeB threeC . runTriple

unpackTripleM :: (Trifunctor f, Functor m) => Triple f (m (Three a b c)) -> f (m a) (m b) (m c)
unpackTripleM = trimap (fmap threeA) (fmap threeB) (fmap threeC) . runTriple

goTriple cell = case cell of
  ThreeA run -> fmap ThreeA . run . unpackTripleM
  ThreeB run -> fmap ThreeB . run . unpackTripleM
  ThreeC run -> fmap ThreeC . run . unpackTripleM

newtype Loeb3 f s a b c
  = Loeb3
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) -> ST s c)
      )

loeb3 ::
  (Tritraversable f) =>
  Loeb3 f s a b c ->
  f a b c
loeb3 (Loeb3 spreadsheet) = unpackTriple $ loeb $ Loeb $ fmap goTriple <$> packTripleM spreadsheet

loebST3 ::
  (Tritraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) -> ST s c) ->
  ST s (t a b c)
loebST3 spreadsheet = fmap unpackTriple $ loebST $ fmap goTriple <$> packTripleM spreadsheet

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

packQuadrupleM ::
  (Quadrifunctor f, Functor m) =>
  f (m a) (m b) (m c) (m d) ->
  Quadruple f (m (Four a b c d))
packQuadrupleM =
  Quadruple
    . quadrimap
      (fmap FourA)
      (fmap FourB)
      (fmap FourC)
      (fmap FourD)

unpackQuadruple ::
  (Quadrifunctor t) =>
  Quadruple t (Four a b c d) ->
  t a b c d
unpackQuadruple =
  quadrimap
    fourA
    fourB
    fourC
    fourD
    . runQuadruple

unpackQuadrupleM ::
  (Quadrifunctor f, Functor m) =>
  Quadruple f (m (Four a b c d)) ->
  f (m a) (m b) (m c) (m d)
unpackQuadrupleM =
  quadrimap
    (fmap fourA)
    (fmap fourB)
    (fmap fourC)
    (fmap fourD)
    . runQuadruple

goQuadruple cell = case cell of
  FourA run -> fmap FourA . run . unpackQuadrupleM
  FourB run -> fmap FourB . run . unpackQuadrupleM
  FourC run -> fmap FourC . run . unpackQuadrupleM
  FourD run -> fmap FourD . run . unpackQuadrupleM

newtype Loeb4 f s a b c d
  = Loeb4
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s c)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) -> ST s d)
      )

loeb4 ::
  (Quadritraversable f) =>
  Loeb4 f s a b c d ->
  f a b c d
loeb4 (Loeb4 spreadsheet) = unpackQuadruple $ loeb $ Loeb $ fmap goQuadruple <$> packQuadrupleM spreadsheet

loebST4 ::
  (Quadritraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s d) ->
  ST s (t a b c d)
loebST4 spreadsheet = fmap unpackQuadruple $ loebST $ fmap goQuadruple <$> packQuadrupleM spreadsheet

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

packQuintupleM ::
  (Pentafunctor f, Functor m) =>
  f (m a) (m b) (m c) (m d) (m e) ->
  Quinetuple f (m (Five a b c d e))
packQuintupleM =
  Quinetuple
    . pentamap
      (fmap FiveA)
      (fmap FiveB)
      (fmap FiveC)
      (fmap FiveD)
      (fmap FiveE)

unpackQuintuple ::
  (Pentafunctor t) =>
  Quinetuple t (Five a b c d e) ->
  t a b c d e
unpackQuintuple =
  pentamap
    fiveA
    fiveB
    fiveC
    fiveD
    fiveE
    . runQuinetuple

unpackQuintupleM ::
  (Pentafunctor f, Functor m) =>
  Quinetuple f (m (Five a b c d e)) ->
  f (m a) (m b) (m c) (m d) (m e)
unpackQuintupleM =
  pentamap
    (fmap fiveA)
    (fmap fiveB)
    (fmap fiveC)
    (fmap fiveD)
    (fmap fiveE)
    . runQuinetuple

goQuintuple cell = case cell of
  FiveA run -> fmap FiveA . run . unpackQuintupleM
  FiveB run -> fmap FiveB . run . unpackQuintupleM
  FiveC run -> fmap FiveC . run . unpackQuintupleM
  FiveD run -> fmap FiveD . run . unpackQuintupleM
  FiveE run -> fmap FiveE . run . unpackQuintupleM

newtype Loeb5 f s a b c d e
  = Loeb5
      ( forall s.
        f
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s a)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s b)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s c)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s d)
          (Void, f (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s e)
      )

loeb5 ::
  (Pentatraversable f) =>
  Loeb5 f s a b c d e ->
  f a b c d e
loeb5 (Loeb5 spreadsheet) = unpackQuintuple $ loeb $ Loeb $ fmap goQuintuple <$> packQuintupleM spreadsheet

loebST5 ::
  (Pentatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s e) ->
  ST s (t a b c d e)
loebST5 spreadsheet = fmap unpackQuintuple $ loebST $ fmap goQuintuple <$> packQuintupleM spreadsheet

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

packSixtupleM ::
  (Hexafunctor f', Functor m) =>
  f' (m a) (m b) (m c) (m d) (m e) (m f) ->
  Sixtuple f' (m (Six a b c d e f))
packSixtupleM =
  Sixtuple
    . hexamap
      (fmap SixA)
      (fmap SixB)
      (fmap SixC)
      (fmap SixD)
      (fmap SixE)
      (fmap SixF)

unpackSixtuple ::
  (Hexafunctor t) =>
  Sixtuple t (Six a b c d e f) ->
  t a b c d e f
unpackSixtuple =
  hexamap
    sixA
    sixB
    sixC
    sixD
    sixE
    sixF
    . runSixtuple

unpackSixtupleM ::
  (Hexafunctor f', Functor m) =>
  Sixtuple f' (m (Six a b c d e f)) ->
  f' (m a) (m b) (m c) (m d) (m e) (m f)
unpackSixtupleM =
  hexamap
    (fmap sixA)
    (fmap sixB)
    (fmap sixC)
    (fmap sixD)
    (fmap sixE)
    (fmap sixF)
    . runSixtuple

goSixtuple cell = case cell of
  SixA run -> fmap SixA . run . unpackSixtupleM
  SixB run -> fmap SixB . run . unpackSixtupleM
  SixC run -> fmap SixC . run . unpackSixtupleM
  SixD run -> fmap SixD . run . unpackSixtupleM
  SixE run -> fmap SixE . run . unpackSixtupleM
  SixF run -> fmap SixF . run . unpackSixtupleM

newtype Loeb6 f' s a b c d e f
  = Loeb6
      ( forall s.
        f'
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s a)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s b)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s c)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s d)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s e)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s f)
      )

loeb6 ::
  (Hexatraversable f') =>
  Loeb6 f' s a b c d e f ->
  f' a b c d e f
loeb6 (Loeb6 spreadsheet) = unpackSixtuple $ loeb $ Loeb $ fmap goSixtuple <$> packSixtupleM spreadsheet

loebST6 ::
  (Hexatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s e)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s f) ->
  ST s (t a b c d e f)
loebST6 spreadsheet = fmap unpackSixtuple $ loebST $ fmap goSixtuple <$> packSixtupleM spreadsheet

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

packSeptupleM ::
  (Heptafunctor f', Functor m) =>
  f' (m a) (m b) (m c) (m d) (m e) (m f) (m g) ->
  Septuple f' (m (Seven a b c d e f g))
packSeptupleM =
  Septuple
    . heptamap
      (fmap SevenA)
      (fmap SevenB)
      (fmap SevenC)
      (fmap SevenD)
      (fmap SevenE)
      (fmap SevenF)
      (fmap SevenG)

unpackSeptuple ::
  (Heptafunctor t) =>
  Septuple t (Seven a b c d e f g) ->
  t a b c d e f g
unpackSeptuple =
  heptamap
    sevenA
    sevenB
    sevenC
    sevenD
    sevenE
    sevenF
    sevenG
    . runSeptuple

unpackSeptupleM ::
  (Heptafunctor f', Functor m) =>
  Septuple f' (m (Seven a b c d e f g)) ->
  f' (m a) (m b) (m c) (m d) (m e) (m f) (m g)
unpackSeptupleM =
  heptamap
    (fmap sevenA)
    (fmap sevenB)
    (fmap sevenC)
    (fmap sevenD)
    (fmap sevenE)
    (fmap sevenF)
    (fmap sevenG)
    . runSeptuple

goSeptuple cell = case cell of
  SevenA run -> fmap SevenA . run . unpackSeptupleM
  SevenB run -> fmap SevenB . run . unpackSeptupleM
  SevenC run -> fmap SevenC . run . unpackSeptupleM
  SevenD run -> fmap SevenD . run . unpackSeptupleM
  SevenE run -> fmap SevenE . run . unpackSeptupleM
  SevenF run -> fmap SevenF . run . unpackSeptupleM
  SevenG run -> fmap SevenG . run . unpackSeptupleM

newtype Loeb7 f' s a b c d e f g
  = Loeb7
      ( forall s.
        f'
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s a)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s b)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s c)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s d)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s e)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s f)
          (Void, f' (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s g)
      )

loeb7 ::
  (Heptatraversable f') =>
  Loeb7 f' s a b c d e f g ->
  f' a b c d e f g
loeb7 (Loeb7 spreadsheet) = unpackSeptuple $ loeb $ Loeb $ fmap goSeptuple <$> packSeptupleM spreadsheet

loebST7 ::
  (Heptatraversable t) =>
  t
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s a)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s b)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s c)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s d)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s e)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s f)
    (Void, t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s g) ->
  ST s (t a b c d e f g)
loebST7 spreadsheet = fmap unpackSeptuple $ loebST $ fmap goSeptuple <$> packSeptupleM spreadsheet
