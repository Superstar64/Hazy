module Stage3.Simple.Constructor where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Check.ConstructorInstance (ConstructorInstance (ConstructorInstance))
import qualified Stage3.Check.ConstructorInstance as ConstructorInstance
import Stage3.Simple.Type (Type)
import qualified Stage3.Simple.Type as Type
import qualified Stage3.Tree.Constructor as Solved
import qualified Stage3.Tree.Entry as Solved.Entry
import qualified Stage3.Tree.Field as Solved.Field
import {-# SOURCE #-} qualified Stage3.Unify as Unify

newtype Constructor scope = Constructor
  { entries :: Strict.Vector (Type scope)
  }
  deriving (Show)

instance Shift Constructor where
  shift = shiftDefault

instance Shift.Functor Constructor where
  map category Constructor {entries} =
    Constructor
      { entries = Shift.map category <$> entries
      }

simplify :: Solved.Constructor scope -> Constructor scope
simplify = \case
  Solved.Constructor {Solved.entries} ->
    Constructor
      { entries = Solved.Entry.entry' <$> entries
      }
  Solved.Record {Solved.fields} ->
    Constructor
      { entries = Solved.Entry.entry' . Solved.Field.entry <$> fields
      }

instanciate :: Strict.Vector (Unify.Type s scope) -> Constructor (Local ':+ scope) -> ConstructorInstance s scope
instanciate fresh Constructor {entries} =
  ConstructorInstance
    { ConstructorInstance.entries = Type.instanciate fresh <$> entries
    }
