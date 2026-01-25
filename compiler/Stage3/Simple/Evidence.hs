module Stage3.Simple.Evidence where

import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Evidence (Evidence (..))

lift :: Evidence scope -> Unify.Evidence s scope
lift = \case
  Variable {variable} -> Unify.variable' variable
  Call {function, arguments} -> Unify.call' (lift function) (lift <$> arguments)
  Super {base, index} -> Unify.super (lift base) index
