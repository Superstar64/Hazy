module Semantic.Tree.Select where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)

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
