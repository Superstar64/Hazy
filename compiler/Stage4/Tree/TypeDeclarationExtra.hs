module Stage4.Tree.TypeDeclarationExtra where

import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.TypeDeclarationExtra as Stage3
import Stage4.Index.Term (mapDefault)
import qualified Stage4.Index.Term as Term
import qualified Stage4.Temporary.Definition as Definition
import Stage4.Tree.Expression (Expression)
import qualified Stage4.Tree.Expression as Expression
import qualified Stage4.Tree.Statements as Statements

data TypeDeclarationExtra scope
  = ADT
  | Class
      { defaults :: !(Strict.Vector (Expression (Local ':+ Local ':+ scope)))
      }
  | Synonym
  | GADT
  deriving (Show)

simplify :: Stage3.TypeDeclarationExtra scope -> TypeDeclarationExtra scope
simplify = \case
  Stage3.ADT -> ADT
  Stage3.Synonym -> Synonym
  Stage3.GADT -> GADT
  Stage3.Class {defaults} ->
    Class
      { defaults = go <$> defaults
      }
    where
      go = \case
        Strict.Just expression -> Definition.desugar $ Definition.simplify expression
        Strict.Nothing -> Expression.Join {statements = Statements.Bottom}

instance Shift TypeDeclarationExtra where
  shift = shiftDefault

instance Shift.Functor TypeDeclarationExtra where
  map = mapDefault

instance Term.Functor TypeDeclarationExtra where
  map category = \case
    ADT -> ADT
    GADT -> GADT
    Synonym -> Synonym
    Class {defaults} ->
      Class
        { defaults = Term.map (Term.Over (Term.Over category)) <$> defaults
        }
