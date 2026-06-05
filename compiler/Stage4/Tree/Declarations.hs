module Stage4.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Normal)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import qualified Stage2.Check.Go.Declarations as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Declaration (Declaration)
import qualified Stage4.Tree.Declaration as Declaration
import Stage4.Tree.Instance (Instance)
import qualified Stage4.Tree.Instance as Instance
import Stage4.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage4.Tree.TypeDeclaration as TypeDeclaration
import Stage4.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage4.Tree.TypeDeclarationExtra as TypeDeclarationExtra

data Declarations scope = Declarations
  { terms :: !(Vector (Declaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

instance Shift Declarations where
  shift = shiftDefault

instance Shift.Functor Declarations where
  map = Shift2.mapDefault

instance Shift2.Functor Declarations where
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

simplify :: Stage3.Declarations locality Normal Check scope -> Declarations scope
simplify
  Stage3.Declarations
    { terms,
      types,
      typeExtras,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = Declaration.simplify <$> terms,
        types = TypeDeclaration.simplify <$> types,
        typeExtras = TypeDeclarationExtra.simplify <$> typeExtras,
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
