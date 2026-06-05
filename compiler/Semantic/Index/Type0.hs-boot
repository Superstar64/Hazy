module Semantic.Index.Type0 where

import {-# SOURCE #-} qualified Semantic.Index.Type as Normal
import Semantic.Scope (Declaration, Environment (..), Global)

data Index scopes where
  Declaration :: !Int -> Index (Declaration ':+ scopes)
  Global :: !Int -> !Int -> Index Global

normal :: Index scopes -> Normal.Index scopes
