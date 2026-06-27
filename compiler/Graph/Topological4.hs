module Graph.Topological4 where

import Control.Monad.ST (ST)

data Formula4 t s a b c d z = Formula4
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) -> ST s z
  }
