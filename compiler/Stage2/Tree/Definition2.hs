module Stage2.Tree.Definition2 where

import Data.Kind (Type)
import Data.Map (Map)
import Stage1.Position (Position)
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeTermVariables
import qualified Stage2.Group.Index.Link.Term as Group.Term
import qualified Stage2.Index.Term as Term
import Stage2.Layout (Group, Layout, Normal)
import Stage2.Locality (Locality)
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

type Definition2 :: Locality -> Source -> Mark -> Layout -> Environment -> Type
data Definition2 locality source mark layout scope where
  Manual :: Definition (Local ':+ scope) -> Definition2 locality Single Annotated layout scope
  Auto :: !(Definition scope) -> Definition2 locality Single Inferred Normal scope
  Piece :: !(Choice scope) -> Definition2 locality Single mark Normal scope
  Shared :: !(RightHandSide scope) -> Definition2 locality Share Inferred Normal scope
  Link :: !(Group.Term.Link locality) -> Definition2 locality source Inferred Group scope
  Group ::
    !(Group.Term.Link locality) ->
    !(Map (Group.Term.Link locality) (Definition2 locality' source Inferred Normal scope)) ->
    Definition2 locality source Inferred Group scope

instance Show (Definition2 locality source mark layout scope) where
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
  showsPrec d (Link link) = showParen (d > 10) $ showString "Link " . showsPrec 11 link
  showsPrec d (Group link set) =
    showParen (d > 10) $
      showString "Group "
        . showsPrec 11 link
        . showString " "
        . showsPrec 11 set

instance Shift (Definition2 locality source layout mark) where
  shift = shiftDefault

instance Shift.Functor (Definition2 locality source layout mark) where
  map category = \case
    Manual definition -> Manual (Shift.map (Shift.Over category) definition)
    Auto definition -> Auto (Shift.map category definition)
    Piece choice -> Piece (Shift.map category choice)
    Shared definition -> Shared (Shift.map category definition)
    Link link -> Link link
    Group link set -> Group link (Shift.map category <$> set)

instance FreeTermVariables (Definition2 locality source layout mark) where
  freeTermVariables target = \case
    Manual definition ->
      freeTermVariables (FreeTermVariables.Over target) definition
    Auto definition -> freeTermVariables target definition
    Piece {} -> []
    Shared definition -> freeTermVariables target definition
    Link {} -> []
    Group _ set -> foldMap (freeTermVariables target) set

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

locality :: Definition2 locality source mark Normal scope -> Definition2 locality' source mark Normal scope
locality = \case
  Manual declaration -> Manual declaration
  Auto declaration -> Auto declaration
  Piece piece -> Piece piece
  Shared definition -> Shared definition
