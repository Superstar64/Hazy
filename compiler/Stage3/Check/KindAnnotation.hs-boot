module Stage3.Check.KindAnnotation where

import qualified Data.Strict.Maybe as Strict
import Stage2.Scope (Environment ((:+)), Local)
import qualified Stage3.Tree.Type as Solved
import qualified Stage4.Tree.Type as Simple (Type)

data KindAnnotation scope
  = Annotation {kind' :: !(Simple.Type scope)}
  | Inferred
  | Synonym
      { kind :: !(Strict.Maybe (Solved.Type scope)),
        kind' :: !(Simple.Type scope),
        definition :: !(Solved.Type (Local ':+ scope)),
        definition' :: !(Simple.Type (Local ':+ scope))
      }
