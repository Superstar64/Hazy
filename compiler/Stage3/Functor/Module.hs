module Stage3.Functor.Module where

import Data.Heptafoldable (Heptafoldable (..))
import Data.Heptafunctor (Heptafunctor (heptamap))
import Data.Heptatraversable (Heptatraversable (..), heptafoldMapDefault, heptamapDefault)
import Stage1.Variable (FullQualifiers ((:..)), Qualifiers ((:.)))
import Stage2.Scope (Global)
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.Module as Stage2 (Module (..))
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import Stage3.Functor.Declarations (Declarations)
import qualified Stage3.Functor.Declarations as Declarations
import Stage3.Functor.Instance.Key (Key)

data Module a b c d e f g = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global a b c d e f g
  }

instance Heptafunctor Module where
  heptamap = heptamapDefault

instance Heptafoldable Module where
  heptafoldMap = heptafoldMapDefault

instance Heptatraversable Module where
  heptatraverse f g h i j k l Module {name, declarations} =
    Module name <$> heptatraverse f g h i j k l declarations

mapWithKey ::
  (Int -> a1 -> a2) ->
  (Int -> b1 -> b2) ->
  (Int -> c1 -> c2) ->
  (Int -> d1 -> d2) ->
  (Int -> e1 -> e2) ->
  (Key Global -> f1 -> f2) ->
  (Key Global -> g1 -> g2) ->
  Module a1 b1 c1 d1 e1 f1 g1 ->
  Module a2 b2 c2 d2 e2 f2 g2
mapWithKey f1 f2 f3 f4 f5 f6 f7 Module {name, declarations} =
  Module
    { name,
      declarations = Declarations.mapWithKey f1 f2 f3 f4 f5 f6 f7 declarations
    }

fromStage2 ::
  Stage2.Module ->
  Module
    (Stage2.TermDeclaration Global)
    (Stage2.TermDeclaration Global)
    (Stage2.TypeDeclaration Global)
    (Stage2.TypeDeclaration Global)
    (Stage2.TypeDeclaration Global)
    (Stage2.Instance Global)
    (Stage2.Instance Global)
fromStage2 Stage2.Module {name = name@(path :.. base), declarations}
  | let root = path :. base =
      Module
        { name,
          declarations = Declarations.fromStage2 root declarations
        }
