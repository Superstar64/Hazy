module Stage2.Tree.MethodConcrete where

import Stage2.Connect (Connect (..))
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Combinators.Implicit (Implicit (..))
import Stage2.Tree.Definition (Definition)

data MethodConcrete layout stage scope
  = Definition
      { definition :: !(Implicit (Definition layout stage) stage (Local ':+ scope))
      }
  | Default {}
  deriving (Show)

instance Shift (MethodConcrete layout stage) where
  shift = shiftDefault

instance Shift.Functor (MethodConcrete layout stage) where
  map category = \case
    Definition {definition} ->
      Definition
        { definition = Shift.map (Shift.Over category) definition
        }
    Default {} -> Default {}

instance Connect MethodConcrete where
  connect = \case
    Definition {definition = Resolve definition} ->
      Definition
        { definition = Resolve (connect definition)
        }
    Default {} -> Default {}
