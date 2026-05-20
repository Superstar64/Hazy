module Stage4.Tree.MethodConcrete where

import qualified Data.Vector as Vector
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.MethodConcrete as Stage3
import qualified Stage4.Shift as Shift2
import Stage4.Substitute (Category (Substitute))
import qualified Stage4.Substitute as Substitute
import qualified Stage4.Tree.Expression as Expression
import {-# SOURCE #-} Stage4.Tree.Expression (Expression)
import Stage4.Tree.SchemeOver (SchemeOver (..))
import qualified Stage4.Tree.SchemeOver as SchemeOver

newtype MethodConcrete scope = Definition
  { definition :: SchemeOver Expression (Local ':+ scope)
  }
  deriving (Show)

instance Shift MethodConcrete where
  shift = shiftDefault

instance Shift.Functor MethodConcrete where
  map = Shift2.mapDefault

instance Shift2.Functor MethodConcrete where
  map = Substitute.mapDefault

instance Substitute.Functor MethodConcrete where
  map category Definition {definition} =
    Definition
      { definition = Substitute.map (Substitute.Over category) definition
      }

simplify :: Stage3.MethodConcrete scope -> MethodConcrete scope
simplify = \case
  Stage3.Definition {definition} ->
    Definition
      { definition = SchemeOver.map (SchemeOver.Map Expression.simplify) definition
      }
  Stage3.Default {base, self, defaultx} ->
    Definition
      { definition =
          let typeReplacements = Vector.singleton base
              evidenceReplacements = Vector.singleton self
              category = Substitute Shift.Shift typeReplacements evidenceReplacements
           in Substitute.map category defaultx
      }
