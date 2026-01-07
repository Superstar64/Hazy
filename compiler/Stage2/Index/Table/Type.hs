module Stage2.Index.Table.Type
  ( Table (..),
    (!),
    Map (..),
    map,
  )
where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Index.Type (Index)
import qualified Stage2.Index.Type as Index
import Stage2.Scope
  ( Declaration,
    Environment (..),
    Global,
    Local,
    Pattern,
  )
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift
import Prelude hiding (map)

type Table :: (Environment -> Type) -> Environment -> Type
data Table value scope where
  Declaration :: Vector (value (Declaration ':+ scope)) -> Table value scope -> Table value (Declaration ':+ scope)
  Pattern :: Table value scope -> Table value (Pattern ':+ scope)
  Local :: Table value scope -> Table value (Local ':+ scope)
  Global :: Vector (Vector (value Global)) -> Table value Global

instance Shift.Unshift (Table value) where
  unshift (Declaration _ table) = table
  unshift (Pattern table) = table
  unshift (Local table) = table

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
Declaration _ table ! Index.Shift index = shift $ table ! index
Pattern table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global values ! Index.Global global local = values Vector.! global Vector.! local

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map value value' = Map (forall scope. value scope -> value' scope)

map :: Map value value' -> Table value scope -> Table value' scope
map (Map f) = \case
  Declaration values table -> Declaration (Vector.map f values) (map (Map f) table)
  Pattern table -> Pattern (map (Map f) table)
  Local table -> Local (map (Map f) table)
  Global values -> Global (Vector.map (Vector.map f) values)
