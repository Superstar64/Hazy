module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import Core.Tree.Evidence (EvidenceF (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import qualified Semantic.Unify.Instanciation as Instanciation

type Evidence s = EvidenceF (Logical s)

data Logical s scope where
  Box :: !(STRef s (Box s scope)) -> Logical s scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

instance Shift0.Functor (Logical s) where
  map = \case
    Shift0.Id -> id
    Shift0.Shift -> Shift

instance Shift.Functor (Logical s) where
  map Shift.Shift logical = Shift logical
  map (Shift.Over category) (Shift logical) = Shift (Shift.map category logical)
  map Shift.Over {} Box {} = error "can't map over logical"
  map _ _ = error "unsuppported shift"

data Box s scope
  = Solved !(EvidenceF (Logical s) scope)
  | Unsolved {}

-- unification between evidence should never fail
unify :: EvidenceF (Logical s) scope -> EvidenceF (Logical s) scope -> ST s ()
unify (Logical (Box reference)) (Logical (Box reference'))
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved evidence) (Solved evidence') = unify evidence evidence'
    combine Unsolved _ =
      writeSTRef reference $ Solved $ Logical (Box reference')
    combine _ Unsolved =
      writeSTRef reference' $ Solved $ Logical (Box reference)
unify (Logical (Box reference)) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify evidence evidence'
    Unsolved -> writeSTRef reference (Solved evidence')
unify evidence (Logical (Box reference')) =
  readSTRef reference' >>= \case
    Solved evidence' -> unify evidence evidence'
    Unsolved -> writeSTRef reference' (Solved evidence)
unify (Variable index instanciation) (Variable index' instanciation')
  | index == index' = do
      Instanciation.unify instanciation instanciation'
unify (Logical (Shift logical)) evidence' = do
  evidence' <- unshift evidence'
  unify (Logical logical) evidence'
unify evidence (Logical (Shift logical')) = do
  evidence <- unshift evidence
  unify evidence (Logical logical')
unify _ _ = error "unify evidence can't fail"

unshift :: EvidenceF (Logical s) (scope ':+ scopes) -> ST s (EvidenceF (Logical s) scopes)
unshift = \case
  Variable variable instanciation -> do
    let fail = error "unshift can't fail"
    instanciation <- Instanciation.unshift instanciation
    pure $ Variable (Shift.map (Shift.Unshift fail) variable) instanciation
  Logical (Box reference) -> do
    readSTRef reference >>= \case
      Solved evidence -> unshift evidence
      Unsolved -> do
        box <- newSTRef Unsolved
        writeSTRef reference (Solved $ Logical $ Shift $ Box box)
        pure $ Logical (Box box)
  Logical (Shift logical) -> pure (Logical logical)
  Super evidence index -> do
    evidence <- unshift evidence
    pure $ Super evidence index
