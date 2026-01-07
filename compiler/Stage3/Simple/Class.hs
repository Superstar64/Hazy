module Stage3.Simple.Class where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Check.ClassInstance (ClassInstance (ClassInstance))
import qualified Stage3.Check.ClassInstance as ClassInstance
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Simple.Constraint (Constraint)
import Stage3.Simple.Scheme (Scheme)
import qualified Stage3.Simple.Scheme as Simple.Scheme
import Stage3.Simple.Type (Type)
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }
  deriving (Show)

instance Shift Class where
  shift = shiftDefault

instance Shift.Functor Class where
  map category Class {parameter, constraints, methods} =
    Class
      { parameter = Shift.map category parameter,
        constraints = Shift.map category <$> constraints,
        methods = Shift.map (Shift.Over category) <$> methods
      }

kind :: Class scope -> Type scope
kind Class {parameter} = Type.Function parameter Type.Constraint

instanciate :: Context s scope -> Position -> Type2.Index scope -> Class scope -> ST s (ClassInstance s scope)
instanciate context position index Class {parameter, methods} = do
  typex <- Unify.fresh (Type.lift parameter)
  evidence <- Unify.constrain context position index typex
  let types = Strict.Vector.singleton typex
  methods <- pure $ Simple.Scheme.instanciate' types <$> methods
  pure
    ClassInstance
      { ClassInstance.typex,
        ClassInstance.evidence,
        ClassInstance.methods
      }
