module Stage2.Tree.Definition3 where

import Stage1.Tree.Fixity (Fixity)
import Stage1.Variable (Variable)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Definition2 (Definition2 (..), Share, Single)

data Definition3 mark layout stage scope where
  (::@) :: !(Info source) -> !(Definition2 source mark layout stage scope) -> Definition3 mark layout stage scope

infixr 5 ::@

instance Show (Definition3 mark layout stage scope) where
  showsPrec d (info ::@ definition) =
    showParen (d > 5) $
      showsPrec 6 info . showString " ::@ " . showsPrec 6 definition

instance Shift (Definition3 mark layout stage) where
  shift = shiftDefault

instance Shift.Functor (Definition3 mark layout stage) where
  map category (info ::@ definition) = info ::@ Shift.map category definition

instance FreeTermVariables (Definition3 mark layout) where
  freeTermVariables target (_ ::@ definition) = freeTermVariables target definition

instance Connect (Definition3 mark) where
  connect (info ::@ definition) = info ::@ connect definition

data Info source where
  Name :: !Variable -> !Fixity -> Info Single
  Unnamed :: !Int -> Info Share

instance Show (Info source) where
  showsPrec d info = showParen (d > 10) $ case info of
    Name name fixity -> showString "Name " . showsPrec 11 name . showString " " . showsPrec 11 fixity
    Unnamed index -> showString "Unnamed " . showsPrec 11 index
