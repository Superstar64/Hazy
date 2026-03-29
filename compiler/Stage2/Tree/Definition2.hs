module Stage2.Tree.Definition2 where

import Stage1.Position (Position)
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Annotation (Annotated, Inferred)
import Stage2.Tree.Definition (Definition)
import Stage2.Tree.Pattern (Pattern)

data Definition2 mark scope where
  Manual :: Definition (Local ':+ scope) -> Definition2 Annotated scope
  Auto :: !(Definition scope) -> Definition2 Inferred scope
  Share :: !(Choice scope) -> Definition2 mark scope

instance Show (Definition2 mark scope) where
  showsPrec d (Manual definition) =
    showParen (d > 10) $
      showString "Manual " . showsPrec 11 definition
  showsPrec d (Auto definition) =
    showParen (d > 10) $
      showString "Auto " . showsPrec 11 definition
  showsPrec d (Share choice) =
    showParen (d > 10) $ showString "Share " . showsPrec 11 choice

instance Shift (Definition2 mark) where
  shift = shiftDefault

instance Shift.Functor (Definition2 mark) where
  map category = \case
    Manual definition -> Manual (Shift.map (Shift.Over category) definition)
    Auto definition -> Auto (Shift.map category definition)
    Share choice -> Share (Shift.map category choice)

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
