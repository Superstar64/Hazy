module Stage2.Check.Temporary.Select where

import Stage2.Check.Temporary.Expression (Expression)

data Select s scope
  = Select !Int !(Expression s scope)
