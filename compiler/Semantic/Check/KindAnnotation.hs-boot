module Semantic.Check.KindAnnotation where

import qualified Core.Tree.Type as Simple (Type)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Check)
import qualified Semantic.Tree.Type as Solved
import qualified Semantic.Tree.TypePattern as Solved
import Syntax.Position (Position)

data KindAnnotation scope
  = Annotation
      { annotation :: !(Solved.Type Position Check scope),
        kind :: !(Simple.Type scope)
      }
  | Inferred
  | Synonym
      { annotation' :: !(Strict.Maybe (Solved.Type Position Check scope)),
        kind :: !(Simple.Type scope),
        parameters :: !(Strict.Vector (Solved.TypePattern Position Check scope)),
        synonym :: !(Solved.Type Position Check (Local ':+ scope))
      }
