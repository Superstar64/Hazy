module Stage4.Substitute where

import Data.Vector (Vector)
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import qualified Stage2.Shift as Shift
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

instance Functor Type.Index where
  map (Lift category) index = Shift2.map category index
  map (Substitute category _ _) index = Shift.map category $ Type.unlocal index
  map (Over category) (Type.Shift index) = Type.Shift $ map category index
  map Over {} (Type.Declaration index) = Type.Declaration index

instance Functor Type2.Index where
  map = Type2.map . map

mapDefault :: (Functor typex) => Shift2.Category scope1 scope2 -> typex scope1 -> typex scope2
mapDefault = map . Lift
