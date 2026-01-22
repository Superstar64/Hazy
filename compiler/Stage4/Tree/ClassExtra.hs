module Stage4.Tree.ClassExtra where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Definition as Stage3
import Stage4.Index.Term (mapDefault)
import qualified Stage4.Index.Term as Term
import qualified Stage4.Temporary.Definition as Definition
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
  map = mapDefault

instance Term.Functor ClassExtra where
  map category ClassExtra {defaults} =
    ClassExtra
      { defaults = Term.map (Term.Over (Term.Over category)) <$> defaults
      }

simplify :: Strict.Vector (Strict.Maybe (Stage3.Definition (Local ':+ (Local ':+ scope)))) -> ClassExtra scope
simplify defaults = ClassExtra {defaults = go <$> defaults}
  where
    go = \case
      Strict.Just expression -> Definition.desugar $ Definition.simplify expression
      Strict.Nothing -> Expression.Join {statements = Statements.Bottom}
