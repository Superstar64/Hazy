module Semantic.Tree.MethodConcrete where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Type as Simple (Type)
import qualified Core.Tree.TypeLambda as Simple (TypeLambda)
import Semantic.Connect (Connect (..))
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Definition (Definition)

data MethodConcrete layout stage scope
  = Definition
      { definition :: !(Implicit Definition layout stage (Local ':+ scope))
      }
  | Default
      { base :: !(Inferred Simple.Type stage (Local ':+ scope)),
        self :: !(Inferred Simple.Evidence stage (Local ':+ scope)),
        defaultx :: !(Inferred (Simple.TypeLambda) stage (Local ':+ scope))
      }
  deriving (Show)

instance Shift0.Functor (MethodConcrete layout stage) where
  map = Shift.mapDefault

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
    Definition {definition} ->
      Definition
        { definition = connect definition
        }
    Default {} ->
      Default
        { base = Inferred,
          self = Inferred,
          defaultx = Inferred
        }
  seperate = \case
    Definition {definition} ->
      Definition
        { definition = seperate definition
        }
    Default {base, self, defaultx} ->
      Default
        { base,
          self,
          defaultx
        }
