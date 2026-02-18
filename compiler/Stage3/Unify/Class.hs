module Stage3.Unify.Class where

import Control.Monad.ST (ST)
import qualified Data.Kind
import Data.STRef (STRef)
import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage3.Unify.Type (Box, Type)
import Prelude hiding (Functor, map)

data Substitute s scope scope' where
  Substitute :: !(Strict.Vector (Type s scope)) -> Substitute s (Scope.Local ':+ scope) scope

class Instantiatable typex where
  substitute :: Substitute s scope scope' -> typex s scope -> typex s scope'

-- |
-- This type used to help make sure zonks are type safe.
--
-- Conceptually, zonks could, in theory, be fully solving an AST while still
-- leaving it in the unsolved AST format. In which case, the state token would
-- be transformed into a fictional `Void` token
--
-- This isn't done at the moment however, hence the single Refl constructor.
--
-- Additionally, this also stops outside use of zonk.
data Zonker s s' where
  Zonker :: Zonker s s

type Zonk :: (Data.Kind.Type -> Environment -> Data.Kind.Type) -> Data.Kind.Constraint
class Zonk typex where
  zonk :: Zonker s s' -> typex s scope -> ST s (typex s' scope)

-- todo, this is O(n^2) due to STRefs not having an order
-- especially not a heterogeneous order

data Collected s scopes where
  Collect :: STRef s (Box s scopes) -> Collected s scopes
  Reach :: Collected s scopes -> Collected s (scope ':+ scopes)

instance Eq (Collected s scopes) where
  Collect left == Collect right = left == right
  Reach left == Reach right = left == right
  _ == _ = False

-- |
-- See rational of Zonker
data Collector s s' where
  Collector :: Collector s s

class (Zonk typex) => Generalizable typex where
  collect :: Collector s s' -> typex s scopes -> ST s [Collected s' scopes]

data Category scope scope' where
  Shift :: Category scopes (scope ':+ scopes)
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')

general :: Category scope scope' -> Shift.Category scope scope'
general Shift = Shift.Shift
general (Over category) = Shift.Over (general category)

class (Shift typex) => Functor typex where
  map :: Category scope scope' -> typex scope -> typex scope'

shiftDefault :: (Functor typex) => typex scopes -> typex (scope ':+ scopes)
shiftDefault = map Shift
