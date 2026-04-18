module Stage2.Group.Functor.Type.Declarations where

import Data.Vector (Vector)

newtype Declarations a = Declarations
  { types :: Vector a
  }
