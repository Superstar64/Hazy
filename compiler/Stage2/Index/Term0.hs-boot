module Stage2.Index.Term0 where

import Stage2.Scope (Declaration, Environment (..), Global)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Global :: !Int -> !Int -> Index Global
