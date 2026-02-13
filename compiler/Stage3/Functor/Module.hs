module Stage3.Functor.Module where

import Data.Octafoldable (Octafoldable (..))
import Data.Octafunctor (Octafunctor (octamap))
import Data.Octatraversable (Octatraversable (..), octafoldMapDefault, octamapDefault)
import Stage1.Variable (FullQualifiers ((:..)), Qualifiers ((:.)))
import Stage2.Scope (Global)
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.Module as Stage2 (Module (..))
import qualified Stage2.Tree.Shared as Stage2 (Shared (..))
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import Stage3.Functor.Declarations (Declarations)
import qualified Stage3.Functor.Declarations as Declarations
import Stage3.Functor.Instance.Key (Key)

data Module a b c d e f g h = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global a b c d e f g h
  }

instance Octafunctor Module where
  octamap = octamapDefault

instance Octafoldable Module where
  octafoldMap = octafoldMapDefault

instance Octatraversable Module where
  octatraverse f g h i j k l m Module {name, declarations} =
    Module name <$> octatraverse f g h i j k l m declarations

mapWithKey ::
  (Int -> a1 -> a2) ->
  (Int -> b1 -> b2) ->
  (Int -> c1 -> c2) ->
  (Int -> d1 -> d2) ->
  (Int -> e1 -> e2) ->
  (Int -> f1 -> f2) ->
  (Key Global -> g1 -> g2) ->
  (Key Global -> h1 -> h2) ->
  Module a1 b1 c1 d1 e1 f1 g1 h1 ->
  Module a2 b2 c2 d2 e2 f2 g2 h2
mapWithKey f1 f2 f3 f4 f5 f6 f7 f8 Module {name, declarations} =
  Module
    { name,
      declarations = Declarations.mapWithKey f1 f2 f3 f4 f5 f6 f7 f8 declarations
    }

fromStage2 ::
  Stage2.Module ->
  Module
    (Stage2.TermDeclaration Global)
    (Stage2.TermDeclaration Global)
    (Stage2.Shared Global)
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
