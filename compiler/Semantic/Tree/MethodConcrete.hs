module Semantic.Tree.MethodConcrete where

import qualified Core.Tree.Evidence as Simple (Evidence)
import {-# SOURCE #-} qualified Core.Tree.Expression as Simple (Expression)
import qualified Core.Tree.SchemeOver as SchemeOver
import qualified Core.Tree.SchemeOver as Simple (SchemeOver)
import qualified Core.Tree.Type as Simple (Type)
import Semantic.Connect (Connect (..))
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Definition (Definition)

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
  seperate = \case
    Definition {definition = Check definition} ->
      Definition
        { definition = Check (SchemeOver.map (SchemeOver.Map seperate) definition)
        }
    Default {base, self, defaultx} ->
      Default
        { base,
          self,
          defaultx
        }
