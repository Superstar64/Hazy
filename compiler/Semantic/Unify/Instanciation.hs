module Semantic.Unify.Instanciation where

import Control.Monad.ST (ST)
import Core.Tree.Instanciation (InstanciationF (..))
import qualified Data.Vector.Strict as Strict.Vector
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment (..))
import {-# SOURCE #-} Semantic.Unify.Evidence (Logical)
import {-# SOURCE #-} qualified Semantic.Unify.Evidence as Evidence
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)

type Instanciation s scope = InstanciationF (Logical s scope) (Type.Logical s scope) scope

unify :: Context s scope -> Position -> Instanciation s scope -> Instanciation s scope -> ST s ()
unify context position (Instanciation instanciation) (Instanciation instanciation') =
  sequence_ $ Strict.Vector.zipWith (Evidence.unify context position) instanciation instanciation'
unify context position Mono {levity = levity1} Mono {levity = levity2} = do
  Type.unify context position levity1 levity2
unify _ _ _ _ = error "unify instanciation can't fail"

unshift ::
  Context s (scope ':+ scopes) ->
  Position ->
  Instanciation s (scope ':+ scopes) ->
  ST s (Instanciation s scopes)
unshift context position = \case
  Instanciation instanciation ->
    Instanciation <$> traverse (Evidence.unshift context position) instanciation
  Mono {levity} -> do
    levity <- Type.unshift context position levity
    pure Mono {levity}
