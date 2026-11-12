module Semantic.Index.Table.Type
  ( Table (..),
    (!),
    Map (..),
    map,
  )
where

import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Semantic.Index.Type (Index)
import qualified Semantic.Index.Type as Index
import Semantic.Scope
  ( Declaration,
    Environment (..),
    Global,
    GroupTerm,
    GroupType,
    Local,
    Pattern,
    SimpleDeclaration,
    SimplePattern,
  )
import Semantic.Shift0 (shift)
import qualified Semantic.Shift0 as Shift0
import Prelude hiding (map)

type Table :: (Environment -> Type) -> Environment -> Type
data Table value scope where
  Declaration :: Vector (value (Declaration ':+ scope)) -> Table value scope -> Table value (Declaration ':+ scope)
  Pattern :: Table value scope -> Table value (Pattern ':+ scope)
  Local :: Table value scope -> Table value (Local ':+ scope)
  Global :: Vector (Vector (value Global)) -> Table value Global
  GroupTerm :: Table value scope -> Table value (GroupTerm ':+ scope)
  GroupType :: Vector (value (GroupType ':+ scope)) -> Table value scope -> Table value (GroupType ':+ scope)
  SimplePattern :: Table value scope -> Table value (SimplePattern ':+ scope)
  SimpleDeclaration :: Table value scope -> Table value (SimpleDeclaration ':+ scope)

instance Shift0.ContraFunctor (Table value) where
  contramap Shift0.Shift (Declaration _ table) = table
  contramap Shift0.Shift (Pattern table) = table
  contramap Shift0.Shift (Local table) = table
  contramap Shift0.Shift (SimplePattern table) = table
  contramap Shift0.Shift (SimpleDeclaration table) = table
  contramap Shift0.Shift (GroupTerm table) = table
  contramap Shift0.Shift (GroupType _ table) = table
  contramap Shift0.Id table = table
  contramap (f Shift0.:. g) table = Shift0.contramap g $ Shift0.contramap f table

(!) :: (Shift0.Functor value) => Table value scope -> Index scope -> value scope
Declaration values _ ! Index.Declaration index = values Vector.! index
Declaration _ table ! Index.Shift index = shift $ table ! index
Pattern table ! Index.Shift index = shift $ table ! index
Local table ! Index.Shift index = shift $ table ! index
Global values ! Index.Global global local = values Vector.! global Vector.! local
SimplePattern table ! Index.Shift index = shift $ table ! index
SimpleDeclaration table ! Index.Shift index = shift $ table ! index
GroupType values _ ! Index.Group index = values Vector.! index
GroupTerm table ! Index.Shift index = shift $ table ! index
GroupType _ table ! Index.Shift index = shift $ table ! index

type Map :: (Environment -> Data.Kind.Type) -> (Environment -> Data.Kind.Type) -> Data.Kind.Type
newtype Map value value' = Map (forall scope. value scope -> value' scope)

map :: Map value value' -> Table value scope -> Table value' scope
map (Map f) = \case
  Declaration values table -> Declaration (Vector.map f values) (map (Map f) table)
  Pattern table -> Pattern (map (Map f) table)
  Local table -> Local (map (Map f) table)
  Global values -> Global (Vector.map (Vector.map f) values)
  SimplePattern table -> SimplePattern (map (Map f) table)
  SimpleDeclaration table -> SimpleDeclaration (map (Map f) table)
  GroupTerm table -> GroupTerm (map (Map f) table)
  GroupType values table -> GroupType (f <$> values) (map (Map f) table)
