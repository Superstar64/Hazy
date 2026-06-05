module Semantic.Check.Functor.Module where

import Data.Heptafoldable (Heptafoldable (..))
import Data.Heptafunctor (Heptafunctor (heptamap))
import Data.Heptatraversable (Heptatraversable (..), heptafoldMapDefault, heptamapDefault)
import Semantic.Check.Functor.Declarations (Declarations)
import qualified Semantic.Check.Functor.Declarations as Declarations
import Semantic.Check.Functor.Instance.Key (Key)
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Global)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declaration as Semantic (Declaration)
import qualified Semantic.Tree.Instance as Semantic (Instance)
import qualified Semantic.Tree.Module as Semantic (Module (..))
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic (TypeDeclarationExtra)
import Syntax.Variable (FullQualifiers ((:..)), Qualifiers ((:.)))

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
  Semantic.Module Group Resolve ->
  Module
    (Semantic.Declaration Locality.Global Group Resolve Global)
    (Semantic.Declaration Locality.Global Group Resolve Global)
    (Semantic.TypeDeclaration Locality.Global Group Resolve Global)
    (Semantic.TypeDeclaration Locality.Global Group Resolve Global)
    (Semantic.TypeDeclarationExtra Group Resolve Global)
    (Semantic.Instance Group Resolve Global)
    (Semantic.Instance Group Resolve Global)
fromStage2 Semantic.Module {name = name@(path :.. base), declarations}
  | let root = path :. base =
      Module
        { name,
          declarations = Declarations.fromStage2 root declarations
        }
