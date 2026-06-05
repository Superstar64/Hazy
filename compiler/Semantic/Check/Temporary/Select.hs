module Semantic.Check.Temporary.Select where

import Semantic.Check.Temporary.Expression (Expression)

data Select s scope
  = Select !Int !(Expression s scope)
