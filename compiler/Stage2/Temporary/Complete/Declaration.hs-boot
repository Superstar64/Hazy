module Stage2.Temporary.Complete.Declaration where

import Stage1.Position (Position)
import Stage1.Tree.Fixity (Fixity)
import Stage2.Layout (Normal)
import qualified Stage2.Temporary.Partial.More.Method as More
import qualified Stage2.Temporary.Partial.More.Selector as More
import Stage2.Tree.Declaration (Key)
import qualified Stage2.Tree.Declaration as Real
import Stage2.Tree.Scheme (Scheme)
import Prelude hiding (Real)

data Real scope
  = Real (forall locality. Real.Declaration locality Normal scope)
  | Select !More.Selector
  | Method !More.Method

data Declaration scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    fixity :: !Fixity,
    annotation :: !(Maybe (Scheme Position scope)),
    declaration :: !(Real scope)
  }
