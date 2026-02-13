module Stage4.Index.Table.Term where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Scope
  ( Declaration,
    Environment (..),
    Global,
    Local,
    SimpleDeclaration,
    SimplePattern,
  )
import Stage2.Shift (Shift (shift))
import Stage4.Index.Term (Index)
import qualified Stage4.Index.Term as Index

type Table :: (Environment -> Type) -> Environment -> Type
data Table value scope where
  Declaration ::
    Vector (value (Declaration ':+ scope)) ->
    Table value scope ->
    Table value (Declaration ':+ scope)
  SimpleDeclaration ::
    value (SimpleDeclaration ':+ scope) ->
    Table value scope ->
    Table value (SimpleDeclaration ':+ scope)
  SimplePattern ::
    Vector (value (SimplePattern ':+ scope)) ->
    Table value scope ->
    Table value (SimplePattern ':+ scope)
  Local :: Table value scope -> Table value (Local ':+ scope)
  Global :: Vector (Vector (value Global)) -> Table value Global

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
SimplePattern values _ ! Index.SimplePattern index = values Vector.! index
SimpleDeclaration value _ ! Index.SimpleDeclaration = value
Declaration _ table ! Index.Shift index = shift $ table ! index
SimplePattern _ table ! Index.Shift index = shift $ table ! index
SimpleDeclaration _ table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global table ! Index.Global global local = table Vector.! global Vector.! local
