module Stage2.Index.Table.Local
  ( Table (..),
    (!),
    Map (Map),
    map,
  )
where

import qualified Data.Kind
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Index.Local (Index)
import qualified Stage2.Index.Local as Index
import Stage2.Scope (Declaration, Environment (..), Global, GroupTerm, GroupType, Local, Pattern)
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift (Unshift (..))
import Prelude hiding (map)

data Table value scope where
  Declaration :: Table value scopes -> Table value (Declaration ':+ scopes)
  Pattern :: Table value scopes -> Table value (Pattern ':+ scopes)
  Local :: Vector (value (Local ':+ scopes)) -> Table value scopes -> Table value (Local ':+ scopes)
  Global :: Table value Global
  GroupTerm :: Table value scope -> Table value (GroupTerm ':+ scope)
  GroupType :: Table value scope -> Table value (GroupType ':+ scope)

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
table ! Index.Local index | Local values _ <- table = values Vector.! index
Local _ table ! Index.Shift index = shift $ table ! index
Declaration table ! Index.Shift index = shift $ table ! index
Pattern table ! Index.Shift index = shift $ table ! index
GroupTerm table ! Index.Shift index = shift $ table ! index
GroupType table ! Index.Shift index = shift $ table ! index

instance Shift.Unshift (Table value) where
  unshift (Declaration table) = table
  unshift (Pattern table) = table
  unshift (Local _ table) = table
  unshift (GroupTerm table) = table
  unshift (GroupType table) = table

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map value value' = Map (forall scope. value scope -> value' scope)

map :: Map value value' -> Table value scope -> Table value' scope
map (Map f) = \case
  Declaration table -> Declaration (map (Map f) table)
  Pattern table -> Pattern (map (Map f) table)
  Local values table -> Local (Vector.map f values) (map (Map f) table)
  Global -> Global
  GroupTerm table -> GroupTerm (map (Map f) table)
  GroupType table -> GroupType (map (Map f) table)
