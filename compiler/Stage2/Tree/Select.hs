module Stage2.Tree.Select where

import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)

data Select layout stage scope
  = Select !Int !(Expression layout stage scope)
  deriving (Show)

instance Shift (Select layout stage) where
  shift = shiftDefault

instance Shift.Functor (Select layout stage) where
  map category (Select pick record) =
    Select pick (Shift.map category record)

instance FreeTermVariables (Select layout) where
  freeTermVariables target (Select _ expression) = freeTermVariables target expression

instance Connect Select where
  connect (Select pick record) = Select pick (connect record)
  seperate (Select pick record) = Select pick (seperate record)
