module Stage2.Tree.Body where

import qualified Data.Strict.Vector1 as Strict
import Stage1.Position (Position)
import qualified Stage1.Tree.Body as Stage1 (Body (..))
import Stage2.Resolve.Context (Context)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Statements (Statements)
import qualified Stage2.Tree.Statements as Statements (resolve)

data Body scope
  = Body !(Expression scope)
  | Guards !(Strict.Vector1 (Statements scope))
  deriving (Show)

instance Shift Body where
  shift = shiftDefault

instance Shift.Functor Body where
  map category = \case
    Body expression -> Body (Shift.map category expression)
    Guards statements -> Guards (fmap (Shift.map category) statements)

resolve :: Context scope -> Stage1.Body Position -> Body scope
resolve context = \case
  Stage1.Body {expression} -> Body (Expression.resolve context expression)
  Stage1.Guards {statements} -> Guards (fmap (Statements.resolve context) statements)
