module Semantic.Tree.RightHandSide where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Locality as Locality
import Semantic.Scope (Declaration, Environment ((:+)))
import Semantic.Shift (Shift (shift), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Body (Body)
import {-# SOURCE #-} Semantic.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Semantic.Tree.Declarations as Declarations

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
