module Stage5.Generate.Variable where

import Data.Text (Text)
import Stage5.Generate.Global (Global)

data Variable
  = Local !Text
  | Global !Global
