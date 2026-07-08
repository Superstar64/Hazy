module Semantic.Unify.Evidence where

import Control.Monad.ST (ST)
import qualified Core.Tree.Evidence as Simple (Evidence (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Error (unsupportedFeatureConstraintedTypeDefaulting)
import qualified Semantic.Index.Evidence as Evidence
import Semantic.Scope (Environment (..))
import Semantic.Shift (Shift, shift)
import qualified Semantic.Shift as Shift
import Semantic.Unify.Class (Solve (..), Zonk (..), Zonker (..))
import Semantic.Unify.Instanciation (Instanciation)
import qualified Semantic.Unify.Instanciation as Instanciation
import Syntax.Position (Position)

data Evidence s scope
  = Logical !(Logical s scope)
  | Variable !(Evidence.Index scope) !(Instanciation s scope)
  | Super !(Evidence s scope) !Int

data Logical s scope where
  Box :: !(STRef s (Box s scope)) -> Logical s scope
  Shift :: !(Logical s scopes) -> Logical s (scope ':+ scopes)

instance Shift (Evidence s) where
  shift = \case
    Logical logical -> Logical (Shift logical)
    Variable variable instanciation -> Variable (shift variable) (shift instanciation)
    Super evidence index -> Super (shift evidence) index

instance Zonk Evidence where
  zonk Zonker = \case
    Variable variable instanciation -> do
      instanciation <- zonk Zonker instanciation
      pure $ Variable variable instanciation
    Super evidence index -> do
      evidence <- zonk Zonker evidence
      pure $ Super evidence index
    Logical (Box reference) ->
      readSTRef reference >>= \case
        Solved evidence -> zonk Zonker evidence
        Unsolved {} -> pure $ Logical (Box reference)
    Logical (Shift logical) -> shift <$> zonk Zonker (Logical logical)

data Box s scope
  = Solved !(Evidence s scope)
  | Unsolved {}

-- unification between evidence should never fail
unify :: Evidence s scope -> Evidence s scope -> ST s ()
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

unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
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

solve :: Position -> Evidence s scope -> Solve s (Simple.Evidence scope)
solve position = \case
  Variable variable instanciation -> do
    instanciation <- Instanciation.solve position instanciation
    pure Simple.Variable {variable, instanciation}
  Super base index -> do
    base <- solve position base
    pure $ Simple.Super {base, index}
  Logical (Shift evidence) -> shift <$> solve position (Logical evidence)
  Logical (Box reference) ->
    Solve (readSTRef reference) >>= \case
      Solved evidence -> solve position evidence
      Unsolved -> unsupportedFeatureConstraintedTypeDefaulting position
