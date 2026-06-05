module Generate.Global where

import Data.Text (Text)
import Syntax.Variable (FullQualifiers)

data Global = Global
  { path :: !FullQualifiers,
    name :: !Text
  }
  deriving (Eq, Ord)
