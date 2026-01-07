module Stage4.Index.Table.Term where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Scope (Declaration, Environment (..), Global, Local, Pattern)
import Stage2.Shift (Shift (shift))
import Stage4.Index.Term (Index)
import qualified Stage4.Index.Term as Index

type Table :: (Environment -> Type) -> Environment -> Type
data Table value scope where
  Declaration :: Vector (value (Declaration ':+ scope)) -> Table value scope -> Table value (Declaration ':+ scope)
  Pattern :: Vector (value (Pattern ':+ scope)) -> Table value scope -> Table value (Pattern ':+ scope)
  Local :: Table value scope -> Table value (Local ':+ scope)
  Global :: Vector (Vector (value Global)) -> Table value Global

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
Pattern values _ ! Index.Pattern index = values Vector.! index
Declaration _ table ! Index.Shift index = shift $ table ! index
Pattern _ table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global table ! Index.Global global local = table Vector.! global Vector.! local
