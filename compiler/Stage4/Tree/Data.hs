module Stage4.Tree.Data where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Selector (Selector)
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Constructor (Constructor)
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

data Data scope = Data
  { parameters :: !(Strict.Vector (Type scope)),
    constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
    selectors :: !(Strict.Vector Selector)
  }
  deriving (Show)

instance Shift Data where
  shift = shiftDefault

instance Shift.Functor Data where
  map = Shift2.mapDefault

instance Shift2.Functor Data where
  map = Substitute.mapDefault

instance Substitute.Functor Data where
  map category Data {parameters, constructors, selectors} =
    Data
      { parameters = Substitute.map category <$> parameters,
        constructors = Substitute.map (Substitute.Over category) <$> constructors,
        selectors
      }

kind :: Data scope -> Type scope
kind Data {parameters} = foldr Type.Function Type.smallType parameters
