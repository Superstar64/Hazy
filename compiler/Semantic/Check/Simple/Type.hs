module Semantic.Check.Simple.Type where

import Core.Tree.Type (Type (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Prelude hiding (map)

instanciate :: Strict.Vector (Unify.Type s scope) -> Type (Local ':+ scope) -> Unify.Type s scope
instanciate fresh = \case
  Variable index -> case index of
    Local.Shift index -> Unify.variable index
    Local.Local index -> fresh Strict.Vector.! index
  Constructor index -> Unify.constructor (Type2.map Type.unlocal index)
  Call function argument -> instanciate fresh function `Unify.call` instanciate fresh argument
  Function argument result -> instanciate fresh argument `Unify.function` instanciate fresh result
  Type universe -> Unify.typeWith (instanciate fresh universe)
  Constraint -> Unify.constraint
  Small -> Unify.small
  Large -> Unify.large
  Universe -> Unify.universe
  Levity -> Unify.levity

-- todo remove this ugly hack

instanciate' ::
  Strict.Vector (Unify.Type s scope) ->
  Type (Local ':+ Local ':+ scope) ->
  Unify.Type s (Local ':+ scope)
instanciate' fresh = \case
  Variable index -> case index of
    Local.Shift (Local.Shift index) -> Unify.variable (Local.Shift index)
    Local.Shift (Local.Local index) -> shift $ fresh Strict.Vector.! index
    Local.Local index -> Unify.variable (Local.Local index)
  Constructor index -> Unify.constructor (Type2.map Type.unlocal index)
  Call function argument -> instanciate' fresh function `Unify.call` instanciate' fresh argument
  Function argument result -> instanciate' fresh argument `Unify.function` instanciate' fresh result
  Type universe -> Unify.typeWith (instanciate' fresh universe)
  Constraint -> Unify.constraint
  Small -> Unify.small
  Large -> Unify.large
  Universe -> Unify.universe
  Levity -> Unify.levity

lift :: Type scope -> Unify.Type s scope
lift = instanciate undefined . shift
