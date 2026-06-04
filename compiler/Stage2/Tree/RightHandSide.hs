module Stage2.Tree.RightHandSide where

import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Locality as Locality
import Stage2.Scope (Declaration, Environment ((:+)))
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Body (Body)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations

data RightHandSide layout stage scope
  = RightHandSide
  { body :: !(Body layout stage (Declaration ':+ scope)),
    declarations :: !(Declarations Locality.Local layout stage (Declaration ':+ scope))
  }
  deriving (Show)

instance Shift (RightHandSide layout stage) where
  shift = shiftDefault

instance Shift.Functor (RightHandSide layout stage) where
  map category (RightHandSide body declarations) =
    RightHandSide (Shift.map (Shift.Over category) body) (Shift.map (Shift.Over category) declarations)

instance FreeTermVariables (RightHandSide layout) where
  freeTermVariables target (RightHandSide body declarations) =
    concat
      [ freeTermVariables (FreeVariables.Over target) body,
        freeTermVariables (FreeVariables.Over target) declarations
      ]

instance Connect RightHandSide where
  connect (RightHandSide body declarations) =
    RightHandSide (connect body) (Declarations.connect declarations)
  seperate (RightHandSide body declarations) =
    RightHandSide (seperate body) (Declarations.seperate declarations)
