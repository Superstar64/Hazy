module Generate.Precontext where

import Data.Vector (Vector)
import Generate.Global (Global)
import Generate.GlobalType (GlobalType)

data Precontext = Precontext
  { terms :: !(Vector (Vector Global)),
    types :: !(Vector (Vector GlobalType))
  }
