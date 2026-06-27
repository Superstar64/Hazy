module Graph.Topological7 where

import Control.Monad.ST (ST)

data Formula7 t s a b c d e f g z = Formula7
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) -> ST s z
  }
