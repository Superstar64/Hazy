module Graph.Topological8 where

import Control.Monad.ST (ST)

data Formula8 t s a b c d e f g h z = Formula8
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) (ST s c) (ST s d) (ST s e) (ST s f) (ST s g) (ST s h) -> ST s z
  }
