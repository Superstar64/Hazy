module Stage4.Tree.InstanceMethod where

import qualified Data.Vector as Vector
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.InstanceMethod as Stage3
import qualified Stage4.Shift as Shift2
import Stage4.Substitute (Category (Substitute))
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Tree.Expression as Expression
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import Stage4.Tree.SchemeOver (SchemeOver (..))

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
simplify = \case
  Stage3.Definition {parameters, constraints, definition} ->
    Definition
      { definition = SchemeOver {parameters, constraints, result = Expression.simplify definition}
      }
  Stage3.Default {parameters, constraints, base, self, defaultx} ->
    Definition
      { definition =
          SchemeOver
            { parameters,
              constraints,
              result =
                let typeReplacements = Vector.singleton base
                    evidenceReplacements = Vector.singleton self
                    category = Substitute.Over $ Substitute Shift.Shift typeReplacements evidenceReplacements
                 in Substitute.map category defaultx
            }
      }
