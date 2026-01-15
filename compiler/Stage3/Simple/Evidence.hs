module Stage3.Simple.Evidence (Evidence (..), lift) where

import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Evidence (Evidence (..))

lift :: Evidence scope -> Unify.Evidence s scope
lift = \case
  Proof {proof, arguments} -> Unify.proof proof (fmap lift arguments)
  Super {base, index} -> Unify.super (lift base) index
