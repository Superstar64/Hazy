module Core.Tree.Declarations where

import qualified Core.Substitute as Substitute
import Core.Tree.Declaration (Declaration)
import qualified Core.Tree.Declaration as Declaration
import Core.Tree.Instance (Instance)
import qualified Core.Tree.Instance as Instance
import Core.Tree.TypeDeclaration (TypeDeclaration)
import qualified Core.Tree.TypeDeclaration as TypeDeclaration
import Core.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Core.Tree.TypeDeclarationExtra as TypeDeclarationExtra
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Semantic.Check.Go.Declarations as Semantic
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)

data Declarations scope = Declarations
  { terms :: !(Vector (Declaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

instance Shift0.Functor Declarations where
  map = Shift.mapDefault

instance Shift.Functor Declarations where
  map = Substitute.mapDefault

instance Substitute.Functor Declarations where
  map category Declarations {terms, types, typeExtras, classInstances, dataInstances} =
    Declarations
      { terms = Substitute.map category <$> terms,
        types = Substitute.map category <$> types,
        typeExtras = Substitute.map category <$> typeExtras,
        classInstances =
          Substitute.mapInstances category . fmap (Substitute.map category) <$> classInstances,
        dataInstances =
          Substitute.mapInstances category . fmap (Substitute.map category) <$> dataInstances
      }

simplify :: Semantic.Declarations Identity Void locality Identity Normal Check scope -> Declarations scope
simplify
  Semantic.Declarations
    { terms,
      types,
      typeExtras,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = Declaration.simplify <$> terms,
        types = TypeDeclaration.simplify <$> types,
        typeExtras = TypeDeclarationExtra.simplify . runIdentity . runIdentity <$> typeExtras,
        classInstances = fmap Instance.simplify <$> classInstances,
        dataInstances = fmap Instance.simplify <$> dataInstances
      }

single :: Declaration scope -> Declarations scope
single term =
  Declarations
    { terms = Vector.singleton term,
      types = Vector.empty,
      typeExtras = Vector.empty,
      dataInstances = Vector.empty,
      classInstances = Vector.empty
    }
