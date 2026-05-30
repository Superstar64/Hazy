module Stage2.Tree.Definition3 where

import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Definition2 (..), Share, Single)

data Definition3 mark layout stage scope where
  Label ::
    !(Info source) ->
    !(Definition2 source mark layout stage scope) ->
    Definition3 mark layout stage scope

instance Scope.Show (Definition3 mark layout stage) where
  showsPrec = showsPrec

instance Show (Definition3 mark layout stage scope) where
  showsPrec d (Label info definition) =
    showParen (d > 10) $
      showString "Label "
        . showsPrec 11 info
        . showString " "
        . showsPrec 11 definition

instance Shift (Definition3 mark layout stage) where
  shift = shiftDefault

instance Shift.Functor (Definition3 mark layout stage) where
  map category (Label info definition) = Label info $ Shift.map category definition

instance FreeTermVariables (Definition3 mark layout) where
  freeTermVariables target (Label _ definition) = freeTermVariables target definition

instance Connect (Definition3 mark) where
  connect (Label info definition) = Label info $ connect definition
  seperate (Label info definition) = Label info $ seperate definition

data Info source where
  Name :: !Variable -> !Fixity -> Info Single
  Unnamed :: !Int -> Info Share

instance Show (Info source) where
  showsPrec d info = showParen (d > 10) $ case info of
    Name name fixity -> showString "Name " . showsPrec 11 name . showString " " . showsPrec 11 fixity
    Unnamed index -> showString "Unnamed " . showsPrec 11 index
