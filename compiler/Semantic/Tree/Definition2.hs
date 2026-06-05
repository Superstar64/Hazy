module Semantic.Tree.Definition2 where

import qualified Core.Tree.Instanciation as Simple
import Data.Kind (Type)
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeTermVariables
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Term as Term
import Semantic.Layout (Layout)
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Resolve, Stage)
import qualified Semantic.Tree.Combinators.Inferred as Inferred
import Semantic.Tree.Definition (Definition)
import Semantic.Tree.Pattern (Pattern)
import Semantic.Tree.RightHandSide (RightHandSide)
import Syntax.Position (Position)

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
  seperate = \case
    Definition definition -> Definition (seperate definition)
    Piece choice -> Piece choice
    Shared definition -> Shared (seperate definition)

data Choice stage scope = Choice
  { position :: !Position,
    index :: !(Term.Index scope),
    instanciation :: !(Inferred.Inferred Simple.Instanciation stage scope),
    bound :: !Term.Bound,
    patternx :: Pattern stage scope
  }
  deriving (Show)

instance Shift (Choice stage) where
  shift = shiftDefault

instance Shift.Functor (Choice stage) where
  map category Choice {position, index, instanciation, bound, patternx} =
    Choice
      { position,
        index = Shift.map category index,
        instanciation = Shift.map category instanciation,
        bound,
        patternx = Shift.map category patternx
      }
