module Stage4.Substitute (Category (..), Functor (..), mapDefault, mapInstances) where

import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Selector as Selector
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Shift as Shift
import qualified Stage4.Index.Term as Term
import qualified Stage4.Shift as Shift2
import {-# SOURCE #-} Stage4.Tree.Evidence (Evidence)
import {-# SOURCE #-} Stage4.Tree.Type (Type)
import Prelude hiding (Functor, map)

data Category scope1 scope2 where
  Lift :: Shift2.Category scope1 scope2 -> Category scope1 scope2
  Over :: Category scopes scopes' -> Category (scope1 ':+ scopes) (scope1 ':+ scopes')
  Substitute ::
    Shift.Category scope scope' ->
    Vector (Type scope') ->
    Vector (Evidence scope') ->
    Category (Local ':+ scope) scope'

class (Shift2.Functor typex) => Functor typex where
  map :: Category scope1 scope2 -> typex scope1 -> typex scope2

general :: Category scope1 scope2 -> Shift2.Category scope1 scope2
general = \case
  Lift category -> category
  Over category -> Shift2.Over (general category)
  Substitute general _ _ -> Shift2.Lift (general Shift.:. Shift.Unshift (error "bad general"))

instance Functor Type.Index where
  map = Shift2.map . general

instance Functor Type2.Index where
  map = Shift2.map . general

instance Functor Constructor.Index where
  map = Shift2.map . general

instance Functor Selector.Index where
  map = Shift2.map . general

instance Functor Method.Index where
  map = Shift2.map . general

instance Functor Term.Index where
  map = Shift2.map . general

mapInstances ::
  Category scope scope' ->
  Map.Map (Type2.Index scope) a ->
  Map.Map (Type2.Index scope') a
mapInstances category = Map.mapKeysMonotonic (map category)

mapDefault :: (Functor typex) => Shift2.Category scope1 scope2 -> typex scope1 -> typex scope2
mapDefault = map . Lift
