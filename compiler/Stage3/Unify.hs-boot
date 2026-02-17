{-# LANGUAGE RoleAnnotations #-}

module Stage3.Unify
  ( module Stage3.Unify,
    Evidence,
    Zonk,
    Constraint,
    Instanciation,
    SchemeOver,
    Type,
    fresh,
    solve,
  )
where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import Stage1.Position (Position)
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope
import {-# SOURCE #-} Stage3.Check.Context (Context)
import qualified Stage3.Index.Evidence as Evidence
import {-# SOURCE #-} Stage3.Unify.Class
import {-# SOURCE #-} Stage3.Unify.Constraint hiding (solve)
import Stage3.Unify.Evidence (Evidence)
import {-# SOURCE #-} Stage3.Unify.Instanciation hiding (solve)
import {-# SOURCE #-} Stage3.Unify.SchemeOver
import {-# SOURCE #-} Stage3.Unify.Type

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

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
variable' :: Evidence.Index scope -> Evidence s scope
call' :: Evidence s scope -> Strict.Vector (Evidence s scope) -> Evidence s scope
super :: Evidence s scope -> Int -> Evidence s scope
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
