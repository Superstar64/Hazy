module Core.Tree.Data where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constructor (Constructor)
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Selector (Selector)
import Syntax.Tree.Brand (Brand)

data Data scope = Data
  { parameters :: !(Strict.Vector (Type scope)),
    constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
    selectors :: !(Strict.Vector Selector),
    brand :: !Brand
  }
  deriving (Show)

instance Shift Data where
  shift = shiftDefault

instance Shift.Functor Data where
  map = Shift2.mapDefault

instance Shift2.Functor Data where
  map = Substitute.mapDefault

instance Substitute.Functor Data where
  map category Data {parameters, constructors, selectors, brand} =
    Data
      { parameters = Substitute.map category <$> parameters,
        constructors = Substitute.map (Substitute.Over category) <$> constructors,
        selectors,
        brand
      }

kind :: Data scope -> Type scope
kind Data {parameters} = foldr Type.Function Type.smallType parameters
