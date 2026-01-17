module Stage4.Temporary.Declarations where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Declarations as Stage3
import qualified Stage4.Index.Term as Term
import Stage4.Temporary.Instance (Instance)
import qualified Stage4.Temporary.Instance as Instance
import Stage4.Temporary.TermDeclaration (TermDeclaration)
import qualified Stage4.Temporary.TermDeclaration as TermDeclaration
import qualified Stage4.Tree.Declarations as Real
import Stage4.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage4.Tree.TypeDeclaration as TypeDeclaration

data Declarations scope = Declarations
  { terms :: !(Vector (TermDeclaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

instance Shift Declarations where
  shift = shiftDefault

instance Shift.Functor Declarations where
  map = Term.mapDefault

instance Term.Functor Declarations where
  map category Declarations {terms, types, classInstances, dataInstances}
    | general <- Term.general category =
        Declarations
          { terms = Term.map category <$> terms,
            types = Term.map category <$> types,
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
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = TermDeclaration.simplify <$> terms,
        types = TypeDeclaration.simplify <$> types,
        classInstances = fmap Instance.simplify <$> classInstances,
        dataInstances = fmap Instance.simplify <$> dataInstances
      }

finish :: Declarations scope -> Real.Declarations scope
finish Declarations {terms, types, classInstances, dataInstances} =
  Real.Declarations
    { terms = TermDeclaration.finish <$> terms,
      types,
      classInstances = fmap Instance.finish <$> classInstances,
      dataInstances = fmap Instance.finish <$> dataInstances
    }
