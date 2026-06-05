module Semantic.Resolve.Temporary.Complete.Declaration where

import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Temporary.Partial.More.Method as More
import qualified Semantic.Resolve.Temporary.Partial.More.Selector as More
import Semantic.Stage (Resolve)
import Semantic.Tree.Declaration (Key)
import qualified Semantic.Tree.Declaration as Real
import Semantic.Tree.Scheme (Scheme)
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)
import Prelude hiding (Real)

data Real scope
  = Real (forall locality. Real.Declaration locality Normal Resolve scope)
  | Select !More.Selector
  | Method !More.Method

data Declaration scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    fixity :: !Fixity,
    annotation :: !(Maybe (Scheme Position Resolve scope)),
    declaration :: !(Real scope)
  }
