module Stage2.Check.Simple.Evidence where

import {-# SOURCE #-} qualified Stage2.Unify as Unify
import {-# SOURCE #-} Stage4.Tree.Evidence (Evidence)

lift :: Evidence scope -> Unify.Evidence s scope
