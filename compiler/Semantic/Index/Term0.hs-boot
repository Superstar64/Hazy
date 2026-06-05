module Semantic.Index.Term0 where

import {-# SOURCE #-} qualified Semantic.Index.Term as Normal
import Semantic.Scope (Declaration, Environment (..), Global)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Global :: !Int -> !Int -> Index Global

normal :: Index scope -> Normal.Index scope
