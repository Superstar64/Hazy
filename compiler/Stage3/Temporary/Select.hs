module Stage3.Temporary.Select where

import Stage3.Temporary.Expression (Expression)

data Select s scope
  = Select !Int !(Expression s scope)
