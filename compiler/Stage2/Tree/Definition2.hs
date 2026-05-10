module Stage2.Tree.Definition2 where

import Data.Kind (Type)
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import qualified Stage2.Index.Term as Term
import Stage2.Layout (Layout)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

data Mark
  = Annotated
  | Inferred

type Annotated = 'Annotated

type Inferred = 'Inferred

data Source
  = Single
  | Share

type Single = 'Single

type Share = 'Share

type Definition2 :: Source -> Mark -> Layout -> Environment -> Type
data Definition2 source mark layout scope where
  Manual :: Definition layout (Local ':+ scope) -> Definition2 Single Annotated layout scope
  Auto :: !(Definition layout scope) -> Definition2 Single Inferred layout scope
  Piece :: !(Choice scope) -> Definition2 Single mark layout scope
  Shared :: !(RightHandSide layout scope) -> Definition2 Share Inferred layout scope

instance Show (Definition2 source mark layout scope) where
  showsPrec d (Manual definition) =
    showParen (d > 10) $
      showString "Manual " . showsPrec 11 definition
  showsPrec d (Auto definition) =
    showParen (d > 10) $
      showString "Auto " . showsPrec 11 definition
  showsPrec d (Piece choice) =
    showParen (d > 10) $ showString "Piece " . showsPrec 11 choice
  showsPrec d (Shared definition) =
    showParen (d > 10) $ showString "Shared " . showsPrec 11 definition

instance Shift (Definition2 source mark layout) where
  shift = shiftDefault

instance Shift.Functor (Definition2 source mark layout) where
  map category = \case
    Manual definition -> Manual (Shift.map (Shift.Over category) definition)
    Auto definition -> Auto (Shift.map category definition)
    Piece choice -> Piece (Shift.map category choice)
    Shared definition -> Shared (Shift.map category definition)

instance FreeTermVariables (Definition2 source mark layout) where
  freeTermVariables target = \case
    Manual definition ->
      freeTermVariables (FreeTermVariables.Over target) definition
    Auto definition -> freeTermVariables target definition
    Piece Choice {index} -> freeTermVariables target index
    Shared definition -> freeTermVariables target definition

data Choice scope = Choice
  { position :: !Position,
    index :: !(Term.Index scope),
    bound :: !Term.Bound,
    patternx :: Pattern scope
  }
  deriving (Show)

instance Shift Choice where
  shift = shiftDefault

instance Shift.Functor Choice where
  map category Choice {position, index, bound, patternx} =
    Choice
      { position,
        index = Shift.map category index,
        bound,
        patternx = Shift.map category patternx
      }
