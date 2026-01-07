module Stage5.Generate.Global where

import Data.Text (Text)
import Stage1.Variable (FullQualifiers)

data Global = Global
  { path :: !FullQualifiers,
    name :: !Text
  }
  deriving (Eq, Ord)
