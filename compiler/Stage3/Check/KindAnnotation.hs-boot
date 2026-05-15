module Stage3.Check.KindAnnotation where

import qualified Data.Strict.Maybe as Strict
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Check)
import qualified Stage2.Tree.Type as Solved
import qualified Stage4.Tree.Type as Simple (Type)

data KindAnnotation scope
  = Annotation
      { annotation :: !(Solved.Type Position Check scope),
        kind :: !(Simple.Type scope)
      }
  | Inferred
  | Synonym
      { annotation' :: !(Strict.Maybe (Solved.Type Position Check scope)),
        kind :: !(Simple.Type scope),
        definition :: !(Solved.Type Position Check (Local ':+ scope)),
        definition' :: !(Simple.Type (Local ':+ scope))
      }
