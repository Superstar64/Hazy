module Stage2.Tree.CallHead where

import Error (badRunSTCall)
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term as Term
import qualified Stage2.Index.Term2 as Term2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (Inferred))
import Stage3.Tree.ConstructorInfo (ConstructorInfo)
import Stage3.Tree.MethodInfo (MethodInfo)
import Stage3.Tree.SelectorInfo (SelectorInfo)
import qualified Stage4.Tree.Evidence as Simple (Evidence)
import qualified Stage4.Tree.Instanciation as Simple (Instanciation)

data CallHead stage scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope),
        instanciation :: !(Inferred Simple.Instanciation stage scope)
      }
  | Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope),
        constructorInfo :: !(Inferred ConstructorInfo stage scope)
      }
  | Selector
      { selectorPosition :: !Position,
        selector :: !(Selector.Index scope),
        selectorInfo :: !(Inferred SelectorInfo stage scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope),
        evidence :: !(Inferred Simple.Evidence stage scope),
        instanciation :: !(Inferred Simple.Instanciation stage scope),
        methodInfo :: !(Inferred MethodInfo stage scope)
      }
  deriving (Show)

instance Shift (CallHead stage) where
  shift = shiftDefault

instance Shift.Functor (CallHead stage) where
  map category = \case
    Variable {variablePosition, variable, instanciation} ->
      Variable
        { variablePosition,
          variable = Shift.map category variable,
          instanciation = Shift.map category instanciation
        }
    Constructor {constructorPosition, constructor, constructorInfo} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor,
          constructorInfo = Shift.map category constructorInfo
        }
    Selector {selectorPosition, selector, selectorInfo} ->
      Selector
        { selectorPosition,
          selector = Shift.map category selector,
          selectorInfo = Shift.map category selectorInfo
        }
    Method {methodPosition, method, evidence, instanciation, methodInfo} ->
      Method
        { methodPosition,
          method = Shift.map category method,
          evidence = Shift.map category evidence,
          instanciation = Shift.map category instanciation,
          methodInfo = Shift.map category methodInfo
        }

instance FreeTermVariables CallHead where
  freeTermVariables target = \case
    Variable {variable} -> FreeVariables.term target variable
    Constructor {} -> []
    Selector {} -> []
    Method {} -> []

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
