module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import {-# SOURCE #-} Semantic.Check.Context (Context)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Instanciation as Instanciation
import {-# SOURCE #-} qualified Semantic.Unify.Type as Type
import Syntax.Position (Position)

type Evidence s scope = EvidenceF (Logical s scope) (Type.Logical s scope) scope

data Logical s scope where
  Box :: !(STRef s (Box s scope)) -> Logical s scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

instance Shift0.Functor (Logical s) where
  map category index = case category of
    Shift0.Id -> index
    Shift0.Shift -> Shift index
    after Shift0.:. before -> Shift0.map after (Shift0.map before index)

data Box s scope
  = Solved !(Evidence s scope)
  | Unsolved {}

-- unification between evidence should never directly fail
-- it may fail due to type unification
unify :: Context s scope -> Position -> Evidence s scope -> Evidence s scope -> ST s ()
unify context position (Logical (Box reference)) (Logical (Box reference'))
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved evidence) (Solved evidence') = unify context position evidence evidence'
    combine Unsolved _ =
      writeSTRef reference $ Solved $ Logical (Box reference')
    combine _ Unsolved =
      writeSTRef reference' $ Solved $ Logical (Box reference)
unify context position (Logical (Box reference)) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify context position evidence evidence'
    Unsolved -> writeSTRef reference (Solved evidence')
unify context position evidence (Logical (Box reference')) =
  readSTRef reference' >>= \case
    Solved evidence' -> unify context position evidence evidence'
    Unsolved -> writeSTRef reference' (Solved evidence)
unify context position (Variable index instanciation) (Variable index' instanciation')
  | index == index' = do
      Instanciation.unify context position instanciation instanciation'
unify context position (Logical (Shift logical)) evidence' = do
  evidence' <- unshift context position evidence'
  unify (Shift.unshift context) position (Logical logical) evidence'
unify context position evidence (Logical (Shift logical')) = do
  evidence <- unshift context position evidence
  unify (Shift.unshift context) position evidence (Logical logical')
unify _ _ _ _ = error "unify evidence can't fail"

unshift ::
  Context s (scope ':+ scopes) ->
  Position ->
  Evidence s (scope ':+ scopes) ->
  ST s (Evidence s scopes)
unshift context position = \case
  Variable variable instanciation -> do
    let fail = error "unshift can't fail"
    instanciation <- Instanciation.unshift context position instanciation
    pure $ Variable (Shift.map (Shift.Unshift fail) variable) instanciation
  Logical (Box reference) -> do
    readSTRef reference >>= \case
      Solved evidence -> unshift context position evidence
      Unsolved -> do
        box <- newSTRef Unsolved
        writeSTRef reference (Solved $ Logical $ Shift $ Box box)
        pure $ Logical (Box box)
  Logical (Shift logical) -> pure (Logical logical)
  Super evidence index -> do
    evidence <- unshift context position evidence
    pure $ Super evidence index
