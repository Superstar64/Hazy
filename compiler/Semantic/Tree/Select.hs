module Semantic.Tree.Select where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)

data Select layout stage scope
  = Select
  { pick :: !Int,
    update :: !(Expression layout stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Select layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Select layout stage) where
  map category (Select pick record) =
    Select pick (Shift.map category record)

instance FreeTermVariables (Select layout) where
  freeTermVariables target (Select _ expression) = freeTermVariables target expression

instance Connect Select where
  connect (Select pick record) = Select pick (connect record)
  seperate (Select pick record) = Select pick (seperate record)
