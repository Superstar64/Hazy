module Graph.Topological1 where

import Control.Monad.ST (ST)

data Formula1 t s a = Formula1
  { cycle :: forall a. a,
    run :: t (ST s a) -> ST s a
  }
