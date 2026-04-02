module Stage3.Tree.StrictnessAnnotation where

import Stage3.Tree.Type (Type)

data StrictnessAnnotation scope
  = Lazy
  | Strict
  | Polymorphic
      { levity :: !(Type scope)
      }
  deriving (Show)
