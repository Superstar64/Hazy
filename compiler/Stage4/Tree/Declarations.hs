module Stage4.Tree.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Index.Term as Term
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
  map = Term.mapDefault

instance Term.Functor Declarations where
  map category Declarations {terms, types, typeExtras, classInstances, dataInstances}
    | general <- Term.general category =
        Declarations
          { terms = Term.map category <$> terms,
            types = Term.map category <$> types,
            typeExtras = Term.map category <$> typeExtras,
            classInstances =
              Shift.mapmap general . fmap (Term.map category) <$> classInstances,
            dataInstances =
              Shift.mapmap general . fmap (Term.map category) <$> dataInstances
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
