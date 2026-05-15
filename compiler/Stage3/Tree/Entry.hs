module Stage3.Tree.Entry where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage2.Tree.Type (Type)
import Stage3.Tree.StrictnessAnnotation (StrictnessAnnotation)

data Entry scope = Entry
  { entry :: !(Type Position Check scope),
    strict :: !(StrictnessAnnotation scope)
  }
  deriving (Show)
