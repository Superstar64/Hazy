module Stage4.Tree.Hook where

import Data.Kind (Type)
import qualified Stage2.Index.Method as Method
import Stage2.Scope (Environment (..))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Evidence (Evidence)

type Hook :: Environment -> Type
data Hook scope
  = DefaultNum
      { evidence :: !(Evidence scope),
        num :: !Method.Num
      }
  | DefaultEnum
      { evidence :: !(Evidence scope),
        enum :: !Method.Enum
      }
  | DefaultEq
      { evidence :: !(Evidence scope),
        eq :: !Method.Eq
      }
  deriving (Show)

instance Shift Hook where
  shift = shiftDefault

instance Shift.Functor Hook where
  map = Shift2.mapDefault

instance Shift2.Functor Hook where
  map = Substitute.mapDefault

instance Substitute.Functor Hook where
  map category = \case
    DefaultNum {evidence, num} ->
      DefaultNum
        { evidence = Substitute.map category evidence,
          num
        }
    DefaultEnum {evidence, enum} ->
      DefaultEnum
        { evidence = Substitute.map category evidence,
          enum
        }
    DefaultEq {evidence, eq} ->
      DefaultEq
        { evidence = Substitute.map category evidence,
          eq
        }
