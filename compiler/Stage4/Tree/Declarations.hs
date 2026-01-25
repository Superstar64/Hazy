module Stage4.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Shift as Shift2
import Stage4.Tree.Instance (Instance)
import qualified Stage4.Tree.Instance as Instance
import Stage4.Tree.TermDeclaration (TermDeclaration)
import qualified Stage4.Tree.TermDeclaration as TermDeclaration
import Stage4.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage4.Tree.TypeDeclaration as TypeDeclaration
import Stage4.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage4.Tree.TypeDeclarationExtra as TypeDeclarationExtra

data Declarations scope = Declarations
  { terms :: !(Vector (TermDeclaration scope)),
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
  map category Declarations {terms, types, typeExtras, classInstances, dataInstances} =
    Declarations
      { terms = Shift2.map category <$> terms,
        types = Shift2.map category <$> types,
        typeExtras = Shift2.map category <$> typeExtras,
        classInstances =
          Shift2.mapInstances category . fmap (Shift2.map category) <$> classInstances,
        dataInstances =
          Shift2.mapInstances category . fmap (Shift2.map category) <$> dataInstances
      }

simplify :: Stage3.Declarations scope -> Declarations scope
simplify
  Stage3.Declarations
    { terms,
      types,
      typeExtras,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = TermDeclaration.simplify <$> terms,
        types = TypeDeclaration.simplify <$> types,
        typeExtras = TypeDeclarationExtra.simplify <$> typeExtras,
        classInstances = fmap Instance.simplify <$> classInstances,
        dataInstances = fmap Instance.simplify <$> dataInstances
      }
