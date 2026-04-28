module Stage2.Tree.Definition2 where

import Data.Kind (Type)
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeTermVariables
import qualified Stage2.Index.Term as Term
import Stage2.Locality (Locality)
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition (Definition)
import Stage2.Tree.Pattern (Pattern)

data Mark
  = Annotated
  | Inferred

type Annotated = 'Annotated

type Inferred = 'Inferred

type Definition2 :: Locality -> Mark -> Environment -> Type
data Definition2 locality mark scope where
  Manual :: Definition (Local ':+ scope) -> Definition2 locality Annotated scope
  Auto :: !(Definition scope) -> Definition2 locality Inferred scope
  Piece :: !(Choice scope) -> Definition2 locality mark scope

instance Show (Definition2 locality mark scope) where
  showsPrec d (Manual definition) =
    showParen (d > 10) $
      showString "Manual " . showsPrec 11 definition
  showsPrec d (Auto definition) =
    showParen (d > 10) $
      showString "Auto " . showsPrec 11 definition
  showsPrec d (Piece choice) =
    showParen (d > 10) $ showString "Piece " . showsPrec 11 choice

instance Shift (Definition2 locality mark) where
  shift = shiftDefault

instance Shift.Functor (Definition2 locality mark) where
  map category = \case
    Manual definition -> Manual (Shift.map (Shift.Over category) definition)
    Auto definition -> Auto (Shift.map category definition)
    Piece choice -> Piece (Shift.map category choice)

instance FreeTermVariables (Definition2 locality mark) where
  freeTermVariables target = \case
    Manual definition ->
      freeTermVariables (FreeTermVariables.Over target) definition
    Auto definition -> freeTermVariables target definition
    Piece {} -> []

data Choice scope = Choice
  { position :: !Position,
    shareIndex :: !Int,
    bound :: !Term.Bound,
    patternx :: Pattern scope
  }
  deriving (Show)

instance Shift Choice where
  shift = shiftDefault

instance Shift.Functor Choice where
  map category Choice {position, shareIndex, bound, patternx} =
    Choice
      { position,
        shareIndex,
        bound,
        patternx = Shift.map category patternx
      }

locality :: Definition2 locality mark scope -> Definition2 locality' mark scope
locality = \case
  Manual declaration -> Manual declaration
  Auto declaration -> Auto declaration
  Piece piece -> Piece piece
