module Stage2.Tree.Select where

import Error (mismatchSelectors)
import Stage1.Position (Position)
import qualified Stage1.Tree.ExpressionField as Stage1 (Field (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Variable (QualifiedVariable (..), Qualifiers (..))
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Type2 as Type2
import Stage2.Resolve.Context (Context (..), (!-%), (!-*))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} Stage2.Tree.Expression as Expression (resolve, variablex)

data Select scope
  = Select !Int !(Expression scope)
  deriving (Show)

instance Shift Select where
  shift = shiftDefault

instance Shift.Functor Select where
  map category (Select pick record) =
    Select pick (Shift.map category record)

resolve :: Type2.Index scope -> Context scope -> Stage1.Field Position -> Select scope
resolve typex context = \case
  Stage1.Field {Stage1.variable, Stage1.field} -> make variable (Expression.resolve context field)
  Stage1.Pun {Stage1.variable = variable@(position :@ _ :- localName)} ->
    let index = context !-* position :@ Local :- localName
     in make variable (variablex position index)
  where
    make name@(position :@ _) expression
      | Selector.Index typex' selector <- context !-% name,
        typex == typex' =
          Select selector expression
      | otherwise = mismatchSelectors position
