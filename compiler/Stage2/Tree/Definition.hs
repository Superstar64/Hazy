module Stage2.Tree.Definition where

import Data.List.NonEmpty (NonEmpty (..))
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Function (Function)

data Definition scope
  = Definition !(Function scope)
  | Alternative !(Function scope) !(Definition scope)
  deriving (Show)

instance Shift Definition where
  shift = shiftDefault

instance Shift.Functor Definition where
  map category = \case
    Definition function -> Definition (Shift.map category function)
    Alternative function definition ->
      Alternative
        (Shift.map category function)
        (Shift.map category definition)

merge :: NonEmpty (Function scope) -> Definition scope
merge (definition :| []) = Definition definition
merge (definition1 :| (definition2 : definitions)) =
  Alternative definition1 (merge (definition2 :| definitions))
