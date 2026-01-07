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
import Stage2.Scope (Declaration, Environment (..), Global, Local, Pattern)
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift (Unshift (..))
import Prelude hiding (map)

data Table value scope where
  Declaration :: Table value scopes -> Table value (Declaration ':+ scopes)
  Pattern :: Table value scopes -> Table value (Pattern ':+ scopes)
  Local :: Vector (value (Local ':+ scopes)) -> Table value scopes -> Table value (Local ':+ scopes)
  Global :: Table value Global

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
table ! Index.Local index | Local values _ <- table = values Vector.! index
Local _ table ! Index.Shift index = shift $ table ! index
Declaration table ! Index.Shift index = shift $ table ! index
Pattern table ! Index.Shift index = shift $ table ! index

instance Shift.Unshift (Table value) where
  unshift (Declaration table) = table
  unshift (Pattern table) = table
  unshift (Local _ table) = table

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map value value' = Map (forall scope. value scope -> value' scope)

map :: Map value value' -> Table value scope -> Table value' scope
map (Map f) = \case
  Declaration table -> Declaration (map (Map f) table)
  Pattern table -> Pattern (map (Map f) table)
  Local values table -> Local (Vector.map f values) (map (Map f) table)
  Global -> Global
