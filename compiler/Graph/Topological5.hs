module Graph.Topological5 where

import Control.Monad.ST (ST)

data Formula5 t s a b c d e z = Formula5
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) -> ST s z
  }
