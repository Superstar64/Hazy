module Stage2.Tree.MethodConcrete where

import Stage2.Connect (Connect (..))
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Combinators.Implicit (Implicit (..))
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Definition (Definition)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)
import qualified Stage4.Tree.Type as Simple (Type)

data MethodConcrete layout stage scope
  = Definition
      { definition :: !(Implicit (Definition layout stage) stage (Local ':+ scope))
      }
  | Default
      { base :: !(Inferred Simple.Type stage (Local ':+ scope)),
        self :: !(Inferred Simple.Evidence stage (Local ':+ scope)),
        defaultx :: !(Inferred (Simple.SchemeOver Simple.Expression) stage (Local ':+ scope))
      }
  deriving (Show)

instance Shift (MethodConcrete layout stage) where
  shift = shiftDefault

instance Shift.Functor (MethodConcrete layout stage) where
  map category = \case
    Definition {definition} ->
      Definition
        { definition = Shift.map (Shift.Over category) definition
        }
    Default {base, self, defaultx} ->
      Default
        { base = Shift.map (Shift.Over category) base,
          self = Shift.map (Shift.Over category) self,
          defaultx = Shift.map (Shift.Over category) defaultx
        }

instance Connect MethodConcrete where
  connect = \case
    Definition {definition = Resolve definition} ->
      Definition
        { definition = Resolve (connect definition)
        }
    Default {} ->
      Default
        { base = Inferred,
          self = Inferred,
          defaultx = Inferred
        }
