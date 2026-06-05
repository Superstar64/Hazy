module Semantic.Tree.ExpressionField where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)

data Field layout stage scope
  = Field
  { index :: !Int,
    expression :: !(Expression layout stage scope)
  }
  deriving (Show)

instance Shift (Field layout stage) where
  shift = shiftDefault

instance Shift.Functor (Field layout stage) where
  map category (Field pick record) =
    Field pick (Shift.map category record)

instance FreeTermVariables (Field layout) where
  freeTermVariables target Field {expression} = freeTermVariables target expression

instance Connect Field where
  connect Field {index, expression} =
    Field
      { index,
        expression = connect expression
      }
  seperate Field {index, expression} =
    Field
      { index,
        expression = seperate expression
      }
