module Stage4.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Instance (Instance)
import qualified Stage4.Tree.Instance as Instance
import Stage4.Tree.TermDeclaration (LazyTermDeclaration)
import qualified Stage4.Tree.TermDeclaration as TermDeclaration
import Stage4.Tree.TypeDeclaration (LazyTypeDeclaration)
import qualified Stage4.Tree.TypeDeclaration as TypeDeclaration
import Stage4.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage4.Tree.TypeDeclarationExtra as TypeDeclarationExtra

data Declarations scope = Declarations
  { terms :: !(Vector (LazyTermDeclaration scope)),
    types :: !(Vector (LazyTypeDeclaration scope)),
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

simplify :: (Int -> Term.Index scope) -> Stage3.Declarations scope -> Declarations scope
simplify
  declaration
  Stage3.Declarations
    { terms,
      types,
      typeExtras,
      shared,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = (TermDeclaration.simplify share <$> terms) <> Vector.imap TermDeclaration.simplifyShared shared,
        types = TypeDeclaration.simplify <$> types,
        typeExtras = TypeDeclarationExtra.simplify <$> typeExtras,
        classInstances = fmap Instance.simplify <$> classInstances,
        dataInstances = fmap Instance.simplify <$> dataInstances
      }
    where
      count = length terms
      share index = declaration (count + index)
