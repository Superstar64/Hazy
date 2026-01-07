module Stage2.Index.Table.Term
  ( Table (..),
    Bound (..),
    (!),
    Map (Map),
    map,
  )
where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Stage2.Index.Term (Index)
import qualified Stage2.Index.Term as Index
import Stage2.Scope (Declaration, Environment (..), Global, Local, Pattern)
import Stage2.Shift (Shift (..))
import qualified Stage2.Shift as Shift
import Prelude hiding (map)

type Table :: (Environment -> Type) -> Environment -> Type
data Table value scope where
  Declaration :: Vector (value (Declaration ':+ scope)) -> Table value scope -> Table value (Declaration ':+ scope)
  Pattern :: Bound value (Pattern ':+ scope) -> Table value scope -> Table value (Pattern ':+ scope)
  Local :: Table value scope -> Table value (Local ':+ scope)
  Global :: Vector (Vector (value Global)) -> Table value Global

instance Shift.Unshift (Table value) where
  unshift (Declaration _ table) = table
  unshift (Pattern _ table) = table
  unshift (Local table) = table

type Bound :: (Environment -> Type) -> Environment -> Type
data Bound value scope = Bound
  { at :: !(value scope),
    select :: !(Strict.Vector (Bound value scope))
  }

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
Pattern bound _ ! Index.Pattern index = indexPattern bound index
  where
    indexPattern :: Bound value scope -> Index.Bound -> value scope
    indexPattern Bound {at} Index.At = at
    indexPattern Bound {select} (Index.Select index bound) = indexPattern (select Strict.Vector.! index) bound
Declaration _ table ! Index.Shift index = shift $ table ! index
Pattern _ table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global table ! Index.Global global local = table Vector.! global Vector.! local

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map value value' = Map (forall scope. value scope -> value' scope)

map :: Map value value' -> Table value scope -> Table value' scope
map (Map f) = \case
  Declaration values table -> Declaration (Vector.map f values) (map (Map f) table)
  Pattern bound table -> Pattern (mapBound (Map f) bound) (map (Map f) table)
  Local table -> Local (map (Map f) table)
  Global values -> Global (Vector.map (Vector.map f) values)

mapBound :: Map value value' -> Bound value scope -> Bound value' scope
mapBound (Map f) Bound {at, select} = Bound {at = f at, select = Strict.Vector.map (mapBound (Map f)) select}
