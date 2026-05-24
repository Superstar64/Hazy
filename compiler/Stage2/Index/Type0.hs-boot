module Stage2.Index.Type0 where

import {-# SOURCE #-} qualified Stage2.Index.Type as Normal
import Stage2.Scope (Declaration, Environment (..), Global)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Global :: !Int -> !Int -> Index Global

normal :: Index scopes -> Normal.Index scopes
