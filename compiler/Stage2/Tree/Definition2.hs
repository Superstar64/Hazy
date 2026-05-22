module Stage2.Tree.Definition2 where

import Data.Kind (Type)
import Stage1.Position (Position)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import qualified Stage2.FreeVariables as FreeVariables
import qualified Stage2.Index.Term as Term
import Stage2.Layout (Layout)
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve, Stage)
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

type Definition2 :: Source -> Mark -> Layout -> Stage -> Environment -> Type
data Definition2 source mark layout stage scope where
  Scoped :: Definition layout Resolve (Local ':+ scope) -> Definition2 Single Annotated layout Resolve scope
  Definition :: Definition layout stage scope -> Definition2 Single mark layout stage scope
  Piece :: !(Choice stage scope) -> Definition2 Single mark layout stage scope
  Shared :: !(RightHandSide layout stage scope) -> Definition2 Share Inferred layout stage scope

instance Scope.Show (Definition2 source mark layout stage) where
  showsPrec = showsPrec

instance Show (Definition2 source mark layout stage scope) where
  showsPrec d (Scoped definition) =
    showParen (d > 10) $
      showString "Scoped " . showsPrec 11 definition
  showsPrec d (Definition definition) =
    showParen (d > 10) $
      showString "Definition " . showsPrec 11 definition
  showsPrec d (Piece choice) =
    showParen (d > 10) $ showString "Piece " . showsPrec 11 choice
  showsPrec d (Shared definition) =
    showParen (d > 10) $ showString "Shared " . showsPrec 11 definition

instance Shift (Definition2 source mark stage layout) where
  shift = shiftDefault

instance Shift.Functor (Definition2 source mark stage layout) where
  map category = \case
    Scoped definition -> Scoped (Shift.map (Shift.Over category) definition)
    Definition definition -> Definition (Shift.map category definition)
    Piece choice -> Piece (Shift.map category choice)
    Shared definition -> Shared (Shift.map category definition)

instance FreeTermVariables (Definition2 source mark layout) where
  freeTermVariables target = \case
    Scoped definition ->
      freeTermVariables (FreeTermVariables.Over target) definition
    Definition definition -> freeTermVariables target definition
    Piece Choice {index} -> FreeVariables.term target index
    Shared definition -> freeTermVariables target definition

instance Connect (Definition2 source mark) where
  connect = \case
    Scoped definition -> Scoped (connect definition)
    Definition definition -> Definition (connect definition)
    Piece choice -> Piece choice
    Shared definition -> Shared (connect definition)

data Choice stage scope = Choice
  { position :: !Position,
    index :: !(Term.Index scope),
    bound :: !Term.Bound,
    patternx :: Pattern stage scope
  }
  deriving (Show)

instance Shift (Choice stage) where
  shift = shiftDefault

instance Shift.Functor (Choice stage) where
  map category Choice {position, index, bound, patternx} =
    Choice
      { position,
        index = Shift.map category index,
        bound,
        patternx = Shift.map category patternx
      }
