module Stage4.Tree.ClassExtra where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Definition as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import qualified Stage4.Tree.Statements as Statements

newtype ClassExtra scope = ClassExtra
  { defaults :: Strict.Vector (Expression (Local ':+ Local ':+ scope))
  }
  deriving (Show)

instance Shift ClassExtra where
  shift = shiftDefault

instance Shift.Functor ClassExtra where
  map = Shift2.mapDefault

instance Shift2.Functor ClassExtra where
  map = Substitute.mapDefault

instance Substitute.Functor ClassExtra where
  map category ClassExtra {defaults} =
    ClassExtra
      { defaults = Substitute.map (Substitute.Over (Substitute.Over category)) <$> defaults
      }

simplify :: Strict.Vector (Strict.Maybe (Stage3.Definition (Local ':+ (Local ':+ scope)))) -> ClassExtra scope
simplify defaults = ClassExtra {defaults = go <$> defaults}
  where
    go = \case
      Strict.Just statements -> Expression.simplify statements
      Strict.Nothing -> Expression.Join {statements = Statements.Bottom}
