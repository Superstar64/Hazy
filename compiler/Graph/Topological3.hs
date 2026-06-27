module Graph.Topological3 where

import Control.Monad.ST (ST)

data Formula3 t s a b c z = Formula3
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) -> ST s z
  }
