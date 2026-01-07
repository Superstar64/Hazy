module Stage2.Index.Constructor where

import Data.Functor.Identity (Identity (..))
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (Bool (False, True), map, traverse)

data Index scope = Index
  { typeIndex :: !(Type2.Index scope),
    constructorIndex :: !Int
  }
  deriving (Show, Eq, Ord)

data Bool = False | True
  deriving (Enum, Bounded)

false =
  Index
    { typeIndex = Type2.Bool,
      constructorIndex = fromEnum False
    }

true =
  Index
    { typeIndex = Type2.Bool,
      constructorIndex = fromEnum True
    }

data List = Nil | Cons
  deriving (Enum, Bounded)

nil =
  Index
    { typeIndex = Type2.List,
      constructorIndex = fromEnum Nil
    }

cons =
  Index
    { typeIndex = Type2.List,
      constructorIndex = fromEnum Cons
    }

data Tuple = Tuple
  deriving (Enum, Bounded)

tuple n =
  Index
    { typeIndex = Type2.Tuple n,
      constructorIndex = fromEnum Tuple
    }

data All a = All
  { bool :: Bool -> a,
    list :: List -> a,
    tuplex :: Int -> Tuple -> a
  }

run :: (Type.Index scope -> Int -> a) -> All a -> Index scope -> a
run normal All {bool, list, tuplex} Index {typeIndex, constructorIndex} = case typeIndex of
  Type2.Index typeIndex -> normal typeIndex constructorIndex
  Type2.Bool -> bool (toEnum constructorIndex)
  Type2.List -> list (toEnum constructorIndex)
  Type2.Tuple n -> tuplex n (toEnum constructorIndex)
  _ -> error "bad run constructor"

map :: (Type.Index scope -> Type.Index scope') -> Index scope -> Index scope'
map run = runIdentity . traverse (Identity . run)

traverse :: (Applicative m) => (Type.Index scope -> m (Type.Index scope')) -> Index scope -> m (Index scope')
traverse run Index {typeIndex, constructorIndex} =
  Index <$> Type2.traverse run typeIndex <*> pure constructorIndex

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category = map (Shift.map category)

instance Shift.PartialUnshift Index where
  partialUnshift abort = traverse (Shift.partialUnshift abort)
