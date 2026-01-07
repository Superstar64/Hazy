module Stage5.Generate.Precontext where

import Data.Vector (Vector)
import Stage5.Generate.Global (Global)
import Stage5.Generate.GlobalType (GlobalType)

data Precontext = Precontext
  { terms :: !(Vector (Vector Global)),
    types :: !(Vector (Vector GlobalType))
  }
