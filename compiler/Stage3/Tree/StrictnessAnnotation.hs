module Stage3.Tree.StrictnessAnnotation where

import Stage1.Position (Position)
import Stage2.Stage (Check)
import Stage2.Tree.Type (Type)

data StrictnessAnnotation scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type Position Check scope)
      }
  deriving (Show)
