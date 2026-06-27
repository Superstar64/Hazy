module Graph.Topological2 where

import Control.Monad.ST (ST)

data Formula2 t s a b z = Formula2
  { cycle :: forall a. a,
    run :: t (ST s a) (ST s b) -> ST s z
  }
