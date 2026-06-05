module Semantic.Resolve.Go.RightHandSide where

import Semantic.Layout (Normal)
import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Body as Body (resolve)
import {-# SOURCE #-} qualified Semantic.Resolve.Go.Declarations as Declarations
import Semantic.Stage (Resolve)
import Semantic.Tree.RightHandSide (RightHandSide (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.RightHandSide as Syntax (RightHandSide (..))

resolve :: Context scope -> Syntax.RightHandSide Position -> RightHandSide Normal Resolve scope
resolve context Syntax.RightHandSide {body, declarations}
  | (context, locals) <- Declarations.resolve context declarations =
      RightHandSide (Body.resolve context body) locals
