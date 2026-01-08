module Stage3.Simple.Data where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Selector (Selector)
import Stage3.Check.DataInstance (DataInstance (DataInstance))
import qualified Stage3.Check.DataInstance as DataInstance
import Stage3.Simple.Constructor (Constructor)
import qualified Stage3.Simple.Constructor as Constructor
import Stage3.Simple.Type (Type)
import qualified Stage3.Simple.Type as Type
import {-# SOURCE #-} qualified Stage3.Unify as Unify

data Data scope = Data
  { parameters :: !(Strict.Vector (Type scope)),
    constructors :: !(Strict.Vector (Constructor (Local ':+ scope))),
    selectors :: !(Strict.Vector Selector)
  }
  deriving (Show)

instance Shift Data where
  shift = shiftDefault

instance Shift.Functor Data where
  map category Data {parameters, constructors, selectors} =
    Data
      { parameters = Shift.map category <$> parameters,
        constructors = Shift.map (Shift.Over category) <$> constructors,
        selectors
      }

kind :: Data scope -> Type scope
kind Data {parameters} = foldr Type.Function Type.smallType parameters

instanciate :: Data scope -> ST s (DataInstance s scope)
instanciate Data {parameters, constructors, selectors} = do
  types <- traverse (Unify.fresh . Type.lift) parameters
  pure
    DataInstance
      { types,
        constructors = Constructor.instanciate types <$> constructors,
        selectors
      }
