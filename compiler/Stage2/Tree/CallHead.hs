module Stage2.Tree.CallHead where

import Error (badRunSTCall)
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Term as Term
import qualified Stage2.Index.Term2 as Term2
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift

data CallHead scope
  = Variable
      { variablePosition :: !Position,
        variable :: !(Term.Index scope)
      }
  | Constructor
      { constructorPosition :: !Position,
        constructor :: !(Constructor.Index scope)
      }
  | Selector
      { selectorPosition :: !Position,
        selector :: !(Selector.Index scope)
      }
  | Method
      { methodPosition :: !Position,
        method :: !(Method.Index scope)
      }
  deriving (Show)

instance Shift CallHead where
  shift = shiftDefault

instance Shift.Functor CallHead where
  map category = \case
    Variable {variablePosition, variable} ->
      Variable
        { variablePosition,
          variable = Shift.map category variable
        }
    Constructor {constructorPosition, constructor} ->
      Constructor
        { constructorPosition,
          constructor = Shift.map category constructor
        }
    Selector {selectorPosition, selector} ->
      Selector
        { selectorPosition,
          selector = Shift.map category selector
        }
    Method {methodPosition, method} ->
      Method
        { methodPosition,
          method = Shift.map category method
        }

resolveVariable :: Position -> Term2.Index scope -> CallHead scope
resolveVariable variablePosition@selectorPosition@methodPosition = \case
  Term2.Index variable ->
    Variable
      { variablePosition,
        variable
      }
  Term2.Select selector ->
    Selector
      { selectorPosition,
        selector
      }
  Term2.Method method ->
    Method
      { methodPosition,
        method
      }
  Term2.RunST -> badRunSTCall variablePosition

resolveConstructor :: Position -> Constructor.Index scope -> CallHead scope
resolveConstructor constructorPosition constructor =
  Constructor
    { constructorPosition,
      constructor
    }

resolveTupling :: Position -> Int -> CallHead scope
resolveTupling constructorPosition count =
  Constructor
    { constructorPosition,
      constructor = Constructor.tuple count
    }

resolveCons :: Position -> CallHead scope
resolveCons constructorPosition =
  Constructor
    { constructorPosition,
      constructor = Constructor.cons
    }
