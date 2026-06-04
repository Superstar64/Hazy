module Stage2.Tree.ExpressionField where

import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)

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
