module Stage2.Index.Type2 where

import Data.Functor.Identity (Identity (..))
import {-# SOURCE #-} qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Type as Type1
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Prelude hiding (map, traverse)

data Index scope
  = Index !(Type1.Index scope)
  | Lifted !(Constructor.Index scope)
  | Bool
  | Char
  | ST
  | Arrow
  | List
  | Tuple !Int
  | Integer
  | Int
  | Num
  | Enum
  | Eq
  deriving (Show, Eq, Ord)

instance Shift Index where
  shift = shiftDefault

instance Shift.Functor Index where
  map category = map (Shift.map category)

instance Shift.PartialUnshift Index where
  partialUnshift abort = traverse (Shift.partialUnshift abort)

map :: (Type1.Index scope -> Type1.Index scope') -> Index scope -> Index scope'
map run = runIdentity . traverse (Identity . run)

traverse :: (Applicative m) => (Type1.Index scope -> m (Type1.Index scope')) -> Index scope -> m (Index scope')
traverse run = \case
  Index index -> Index <$> run index
  Lifted index -> Lifted <$> Constructor.traverse run index
  Bool -> pure Bool
  Char -> pure Char
  ST -> pure ST
  Arrow -> pure Arrow
  List -> pure List
  Tuple count -> pure (Tuple count)
  Integer -> pure Integer
  Int -> pure Int
  Num -> pure Num
  Enum -> pure Enum
  Eq -> pure Eq

unlocal :: Index (Local ':+ scope) -> Index scope
unlocal = map Type1.unlocal
