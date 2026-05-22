module Stage2.Tree.Definition where

import Data.List.NonEmpty (NonEmpty (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Function (Function)

data Definition layout stage scope
  = Definition !(Function layout stage scope)
  | Alternative !(Function layout stage scope) !(Definition layout stage scope)
  deriving (Show)

instance Shift (Definition layout stage) where
  shift = shiftDefault

instance Shift.Functor (Definition layout stage) where
  map category = \case
    Definition function -> Definition (Shift.map category function)
    Alternative function definition ->
      Alternative
        (Shift.map category function)
        (Shift.map category definition)

instance FreeTermVariables (Definition layout) where
  freeTermVariables target = \case
    Definition function -> freeTermVariables target function
    Alternative function definition ->
      concat
        [ freeTermVariables target function,
          freeTermVariables target definition
        ]

instance Connect Definition where
  connect = \case
    Definition function -> Definition (connect function)
    Alternative function definition -> Alternative (connect function) (connect definition)

merge :: NonEmpty (Function layout stage scope) -> Definition layout stage scope
merge (definition :| []) = Definition definition
merge (definition1 :| (definition2 : definitions)) =
  Alternative definition1 (merge (definition2 :| definitions))
