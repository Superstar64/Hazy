module Semantic.Unify.Class where

import qualified Control.Monad as Monad
import Control.Monad.ST (ST)
import qualified Data.Kind
import Data.STRef (STRef)
import qualified Data.Vector.Strict as Strict
import Semantic.Check.Mask (Mask)
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import {-# SOURCE #-} Semantic.Unify.Type (Box, Type)
import Prelude hiding (Functor, map)
import qualified Prelude

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
  Collector :: Mask -> Collector s s

class (Zonk typex) => Generalizable typex where
  collect :: Collector s s' -> typex s scopes -> ST s [Collected s' scopes]

newtype Solve s a = Solve (ST s a)

instance Prelude.Functor (Solve s) where
  fmap = Monad.liftM

instance Prelude.Applicative (Solve s) where
  pure a = Solve (pure a)
  (<*>) = Monad.ap

instance Prelude.Monad (Solve s) where
  Solve m >>= f = Solve (m >>= (\(Solve a) -> a) . f)
