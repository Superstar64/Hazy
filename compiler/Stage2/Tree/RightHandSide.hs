module Stage2.Tree.RightHandSide where

import Stage1.Position (Position)
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeVariables
import Stage2.Layout (Normal)
import qualified Stage2.Locality as Locality
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment ((:+)))
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Body (Body)
import qualified Stage2.Tree.Body as Body (resolve)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations

data RightHandSide layout stage scope
  = RightHandSide
      !(Body layout stage (Declaration ':+ scope))
      !(Declarations Locality.Local layout stage (Declaration ':+ scope))
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

resolve :: Context scope -> Stage1.RightHandSide Position -> RightHandSide Normal Resolve scope
resolve context Stage1.RightHandSide {body, declarations}
  | (context, locals) <- Declarations.resolve context declarations =
      RightHandSide (Body.resolve context body) locals
