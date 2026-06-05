{-# LANGUAGE RoleAnnotations #-}

module Semantic.Unify
  ( module Semantic.Unify,
    Evidence,
    Zonk (..),
    Constraint,
    Instanciation,
    SchemeOver,
    Type,
    fresh,
    mark,
    unify,
    Solve,
    solve,
  )
where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import {-# SOURCE #-} Semantic.Check.Context (Context)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Evidence
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift)
import {-# SOURCE #-} Semantic.Unify.Class
import {-# SOURCE #-} Semantic.Unify.Constraint hiding (solve, unify)
import Semantic.Unify.Evidence (Evidence)
import {-# SOURCE #-} Semantic.Unify.Instanciation hiding (solve, unify)
import {-# SOURCE #-} Semantic.Unify.SchemeOver
import {-# SOURCE #-} Semantic.Unify.Type
import Syntax.Position (Position)

newtype Scheme s scope = Scheme
  { runScheme :: SchemeOver Type s scope
  }

instance Shift (Scheme s)

monoScheme :: Type s scope -> Scheme s scope
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

infixr 0 `function`

function :: Type s scope -> Type s scope -> Type s scope
constraint :: Type s scope
levity :: Type s scope
small :: Type s scope
large :: Type s scope
universe :: Type s scope
variable' :: Evidence.Index scope -> Instanciation s scope -> Evidence s scope
super :: Evidence s scope -> Int -> Evidence s scope
instanciation :: Strict.Vector (Evidence s scope) -> Instanciation s scope
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
constraintx ::
  Type2.Index scope ->
  Int ->
  Strict.Vector (Type s (Scope.Local ':+ scope)) ->
  Constraint s scope
instanciate :: Context s scope -> Position -> Scheme s scope -> ST s (Type s scope, Instanciation s scope)
liftST :: ST s a -> Solve s a
runSolve :: Solve s a -> ST s a
