module Core.Tree.ClassExtra where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Statements as Statements
import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.MethodAbstract as Semantic

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

simplify ::
  Strict.Vector (Semantic.MethodAbstract Normal Check (Local ':+ scope)) ->
  ClassExtra scope
simplify defaults = ClassExtra {defaults = go <$> defaults}
  where
    go :: Semantic.MethodAbstract Normal Check scope -> Expression (Local ':+ scope)
    go = \case
      Semantic.DefaultCheck statements -> Expression.simplify statements
      Semantic.Abstract -> Expression.Join {statements = Statements.Bottom}
