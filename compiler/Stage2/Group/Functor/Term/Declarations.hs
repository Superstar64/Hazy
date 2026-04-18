module Stage2.Group.Functor.Term.Declarations where

import Data.Vector (Vector)

data Declarations a = Declarations
  { terms :: !(Vector a),
    shared :: !(Vector a)
  }
