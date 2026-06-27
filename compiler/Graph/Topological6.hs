module Graph.Topological6 where

import Control.Monad.ST (ST)

data Formula6 t s a b c d e f z = Formula6
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) -> ST s z
  }
