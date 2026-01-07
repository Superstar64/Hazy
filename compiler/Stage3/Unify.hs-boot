{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify where

import Control.Monad.ST (ST)
import qualified Data.Kind as Kind (Type)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift)
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Index.Evidence as Evidence
import {-# SOURCE #-} qualified Stage3.Simple.Type as Simple

type role Type nominal nominal

type Type :: Kind.Type -> Environment -> Kind.Type
data Type s scope

instance Shift (Type s)

type role SchemeOver representational nominal nominal

type SchemeOver :: (Kind.Type -> Environment -> Kind.Type) -> Kind.Type -> Environment -> Kind.Type
data SchemeOver typex s scope

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

type role Constraint nominal nominal

type Constraint :: Kind.Type -> Environment -> Kind.Type
data Constraint s scope

type role Evidence nominal nominal

type Evidence :: Kind.Type -> Environment -> Kind.Type
data Evidence s scope

type role Instanciation nominal nominal

type Instanciation :: Kind.Type -> Environment -> Kind.Type
data Instanciation s scope

variable :: Local.Index scope -> Type s scope
constructor :: Type2.Index scope -> Type s scope
call :: Type s scope -> Type s scope -> Type s scope
index :: Type.Index scope -> Type s scope
lifted :: Constructor.Index scope -> Type s scope
arrow :: Type s scope
list :: Type s scope
listWith :: Type s scope -> Type s scope
tuple :: Int -> Type s scope
typex :: Type s scope
kind :: Type s scope
typeWith :: Type s scope -> Type s scope
function :: Type s scope -> Type s scope -> Type s scope
constraint :: Type s scope
small :: Type s scope
large :: Type s scope
universe :: Type s scope
proof :: Evidence.Index scope -> Strict.Vector (Evidence s scope) -> Evidence s scope
super :: Evidence s scope -> Int -> Evidence s scope
fresh :: Type s scope -> ST s (Type s scope)
unify :: Context s scope -> Position -> Type s scope -> Type s scope -> ST s ()
scheme ::
  Strict.Vector (Type s scope) ->
  Strict.Vector (Constraint s scope) ->
  Type s (Scope.Local ':+ scope) ->
  Scheme s scope
schemeOver ::
  Strict.Vector (Type s scope) ->
  Strict.Vector (Constraint s scope) ->
  typex s (Scope.Local ':+ scope) ->
  SchemeOver typex s scope
constrain :: Context s scope -> Position -> Type2.Index scope -> Type s scope -> ST s (Evidence s scope)
constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
instanciate :: Context s scope -> Position -> Scheme s scope -> ST s (Type s scope, Instanciation s scope)
-- todo, have mechinism to ensure solve is the last ST action
solve :: Position -> Type s scope -> ST s (Simple.Type scope)
