module Semantic.Tree.CallHead where

import qualified Core.Tree.Evidence as Simple (Evidence)
import qualified Core.Tree.Instanciation as Simple (Instanciation)
import Data.Kind (Type)
import Semantic.Check.Info.Constructor (ConstructorInfo)
import Semantic.Check.Info.Method (MethodInfo)
import Semantic.Check.Info.Selector (SelectorInfo)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term as Term
import Semantic.Layout (Layout)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)
import Semantic.Tree.Combinators.Inferred (Inferred)
import Syntax.Position (Position)

type CallHead :: Layout -> Stage -> Environment -> Type
data CallHead layout stage scope
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

instance Shift0.Functor (CallHead layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (CallHead layout stage) where
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

instance Connect CallHead where
  connect = layout
  seperate = layout

layout :: CallHead layout stage scope -> CallHead layout' stage scope
layout = \case
  Variable {variablePosition, variable, instanciation} ->
    Variable {variablePosition, variable, instanciation}
  Constructor {constructorPosition, constructor, constructorInfo} ->
    Constructor {constructorPosition, constructor, constructorInfo}
  Selector {selectorPosition, selector, selectorInfo} ->
    Selector {selectorPosition, selector, selectorInfo}
  Method {methodPosition, method, evidence, instanciation, methodInfo} ->
    Method {methodPosition, method, evidence, instanciation, methodInfo}
