module Stage2.Tree.RightHandSide where

import Stage1.Position (Position)
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Declaration, Environment ((:+)))
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Body (Body)
import qualified Stage2.Tree.Body as Body (resolve)
import {-# SOURCE #-} Stage2.Tree.Declarations (Declarations)
import {-# SOURCE #-} qualified Stage2.Tree.Declarations as Declarations

data RightHandSide scope
  = RightHandSide
      !(Body (Declaration ':+ scope))
      !(Declarations (Declaration ':+ scope))
  deriving (Show)

instance Shift RightHandSide where
  shift = shiftDefault

instance Shift.Functor RightHandSide where
  map category (RightHandSide body declarations) =
    RightHandSide (Shift.map (Shift.Over category) body) (Shift.map (Shift.Over category) declarations)

resolve :: Context scope -> Stage1.RightHandSide Position -> RightHandSide scope
resolve context Stage1.RightHandSide {body, declarations}
  | (context, locals) <- Declarations.resolve context declarations =
      RightHandSide (Body.resolve context body) locals
