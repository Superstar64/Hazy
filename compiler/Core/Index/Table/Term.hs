module Core.Index.Table.Term where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Semantic.Index.Term (Index)
import qualified Semantic.Index.Term as Index
import Semantic.Scope
  ( Declaration,
    Environment (..),
    Global,
    Local,
    SimpleDeclaration,
    SimplePattern,
  )
import Semantic.Shift (shift)
import qualified Semantic.Shift0 as Shift0

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

(!) :: (Shift0.Functor value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
SimplePattern values _ ! Index.SimplePattern index = values Vector.! index
SimpleDeclaration value _ ! Index.SimpleDeclaration = value
Declaration _ table ! Index.Shift index = shift $ table ! index
SimplePattern _ table ! Index.Shift index = shift $ table ! index
SimpleDeclaration _ table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global table ! Index.Global global local = table Vector.! global Vector.! local
