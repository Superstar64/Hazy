module Stage3.Index.Table.Evidence0 where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage2.Scope
  ( Declaration,
    Environment (..),
    Global,
    Local,
    Pattern,
    SimpleDeclaration,
    SimplePattern,
  )
import Stage2.Shift (Shift (..))
import Stage3.Index.Evidence0 (Index)
import qualified Stage3.Index.Evidence0 as Index

data Table value scope where
  Assumed :: Vector (value (Local ':+ scopes)) -> Table value scopes -> Table value (Local ':+ scopes)
  Declaration :: Table value scopes -> Table value (Declaration ':+ scopes)
  Pattern :: Table value scopes -> Table value (Pattern ':+ scopes)
  Global :: Table value Global
  SimplePattern :: Table value scopes -> Table value (SimplePattern ':+ scopes)
  SimpleDeclaration :: Table value scopes -> Table value (SimpleDeclaration ':+ scopes)

(!) :: (Shift value) => Table value scope -> Index scope -> value scope
table ! Index.Assumed index | Assumed values _ <- table = values Vector.! index
Assumed _ table ! Index.Shift index = shift $ table ! index
Declaration table ! Index.Shift index = shift $ table ! index
Pattern table ! Index.Shift index = shift $ table ! index
SimplePattern table ! Index.Shift index = shift $ table ! index
SimpleDeclaration table ! Index.Shift index = shift $ table ! index
