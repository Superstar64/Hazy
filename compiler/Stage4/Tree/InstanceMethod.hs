module Stage4.Tree.InstanceMethod where

import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.InstanceMethod as Stage3
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import Stage4.Tree.SchemeOver (SchemeOver)

newtype InstanceMethod scope = Definition
  { definition :: SchemeOver Expression (Local ':+ scope)
  }
  deriving (Show)

instance Shift InstanceMethod where
  shift = shiftDefault

instance Shift.Functor InstanceMethod where
  map = Shift2.mapDefault

instance Shift2.Functor InstanceMethod where
  map = Substitute.mapDefault

instance Substitute.Functor InstanceMethod where
  map category Definition {definition} =
    Definition
      { definition = Substitute.map (Substitute.Over category) definition
      }

simplify :: Stage3.InstanceMethod scope -> InstanceMethod scope
simplify method =
  Definition
    { definition = Stage3.definition' method
    }
