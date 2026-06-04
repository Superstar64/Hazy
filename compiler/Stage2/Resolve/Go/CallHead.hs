module Stage2.Resolve.Go.CallHead where

import Error (badRunSTCall)
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Term2 as Term2
import Stage2.Stage (Resolve)
import Stage2.Tree.CallHead (CallHead (..))
import Stage2.Tree.Combinators.Inferred (Inferred (Inferred))

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
