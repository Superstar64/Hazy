module Semantic.Resolve.Go.CallHead where

import Error (badRunSTCall)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Term2 as Term2
import Semantic.Stage (Resolve)
import Semantic.Tree.CallHead (CallHead (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Inferred))
import Syntax.Position (Position)

resolveVariable :: Position -> Term2.Index scope -> CallHead Resolve scope
resolveVariable variablePosition@selectorPosition@methodPosition = \case
  Term2.Index variable ->
    Variable
      { variablePosition,
        variable,
        instanciation = Inferred
      }
  Term2.Select selector ->
    Selector
      { selectorPosition,
        selector,
        selectorInfo = Inferred
      }
  Term2.Method method ->
    Method
      { methodPosition,
        method,
        evidence = Inferred,
        instanciation = Inferred,
        methodInfo = Inferred
      }
  Term2.RunST -> badRunSTCall variablePosition

resolveConstructor :: Position -> Constructor.Index scope -> CallHead Resolve scope
resolveConstructor constructorPosition constructor =
  Constructor
    { constructorPosition,
      constructor,
      constructorInfo = Inferred
    }

resolveTupling :: Position -> Int -> CallHead Resolve scope
resolveTupling constructorPosition count =
  Constructor
    { constructorPosition,
      constructor = Constructor.tuple count,
      constructorInfo = Inferred
    }

resolveCons :: Position -> CallHead Resolve scope
resolveCons constructorPosition =
  Constructor
    { constructorPosition,
      constructor = Constructor.cons,
      constructorInfo = Inferred
    }
