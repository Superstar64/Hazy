module Stage3.Simple.Evidence where

import {-# SOURCE #-} qualified Stage3.Unify as Unify
import {-# SOURCE #-} Stage4.Tree.Evidence (Evidence)

lift :: Evidence scope -> Unify.Evidence s scope
