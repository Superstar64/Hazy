module Stage2.Tree.Body where

import qualified Data.Strict.Vector1 as Strict
import Stage1.Position (Position)
import qualified Stage1.Tree.Body as Stage1 (Body (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import {-# SOURCE #-} qualified Stage2.Tree.Expression as Expression (resolve)
import Stage2.Tree.Statements (Statements)
import qualified Stage2.Tree.Statements as Statements (Guard, resolve)

data Body layout stage scope
  = Body {body :: !(Expression layout stage scope)}
  | Guards {guards :: !(Strict.Vector1 (Statements Statements.Guard layout stage scope))}
  deriving (Show)

instance Shift (Body layout stage) where
  shift = shiftDefault

instance Shift.Functor (Body layout stage) where
  map category = \case
    Body expression -> Body (Shift.map category expression)
    Guards statements -> Guards (fmap (Shift.map category) statements)

instance FreeTermVariables (Body layout) where
  freeTermVariables target = \case
    Body expression -> freeTermVariables target expression
    Guards statements -> foldMap (freeTermVariables target) statements

instance Connect Body where
  connect = \case
    Body expression -> Body (connect expression)
    Guards statements -> Guards (fmap connect statements)

resolve :: Context scope -> Stage1.Body Position -> Body Normal Resolve scope
resolve context = \case
  Stage1.Body {expression} -> Body (Expression.resolve context expression)
  Stage1.Guards {statements} -> Guards (fmap (Statements.resolve context) statements)
