module Stage2.Resolve.Go.RightHandSide where

import Stage1.Position (Position)
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import qualified Stage2.Resolve.Go.Body as Body (resolve)
import {-# SOURCE #-} qualified Stage2.Resolve.Go.Declarations as Declarations
import Stage2.Stage (Resolve)
import Stage2.Tree.RightHandSide (RightHandSide (..))

resolve :: Context scope -> Stage1.RightHandSide Position -> RightHandSide Normal Resolve scope
resolve context Stage1.RightHandSide {body, declarations}
  | (context, locals) <- Declarations.resolve context declarations =
      RightHandSide (Body.resolve context body) locals
