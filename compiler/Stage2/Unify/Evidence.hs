module Stage2.Unify.Evidence where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Error (unsupportedFeatureConstraintedTypeDefaulting)
import Stage1.Position (Position)
import qualified Stage2.Index.Evidence as Evidence
import Stage2.Scope (Environment (..))
import Stage2.Shift (shift)
import qualified Stage2.Shift as Shift
import Stage2.Unify.Class (Solve (..), Zonk (..), Zonker (..))
import Stage2.Unify.Instanciation (Instanciation)
import qualified Stage2.Unify.Instanciation as Instanciation
import qualified Stage4.Tree.Evidence as Simple (Evidence (..))

data Evidence s scope where
  Variable :: !(Evidence.Index scope) -> !(Instanciation s scope) -> Evidence s scope
  Super :: !(Evidence s scope) -> !Int -> Evidence s scope
  Logical :: !(STRef s (Box s scope)) -> Evidence s scope
  Shift :: !(Evidence s scopes) -> Evidence s (scope ':+ scopes)

instance Zonk Evidence where
  zonk Zonker = \case
    Variable variable instanciation -> do
      instanciation <- zonk Zonker instanciation
      pure $ Variable variable instanciation
    Super evidence index -> do
      evidence <- zonk Zonker evidence
      pure $ Super evidence index
    Logical box ->
      readSTRef box >>= \case
        Solved evidence -> zonk Zonker evidence
        Unsolved {} -> pure $ Logical box
    Shift evidence -> Shift <$> zonk Zonker evidence

data Box s scope
  = Solved !(Evidence s scope)
  | Unsolved {}

-- unification between evidence should never fail
unify :: Evidence s scope -> Evidence s scope -> ST s ()
unify (Logical reference) (Logical reference')
  | reference == reference' = pure ()
  | otherwise = do
      box <- readSTRef reference
      box' <- readSTRef reference'
      combine box box'
  where
    combine (Solved evidence) (Solved evidence') = unify evidence evidence'
    combine Unsolved _ =
      writeSTRef reference $ Solved $ Logical reference'
    combine _ Unsolved =
      writeSTRef reference' $ Solved $ Logical reference
unify (Logical reference) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify evidence evidence'
    Unsolved -> writeSTRef reference (Solved evidence')
unify evidence (Logical reference') =
  readSTRef reference' >>= \case
    Solved evidence' -> unify evidence evidence'
    Unsolved -> writeSTRef reference' (Solved evidence)
unify (Variable index instanciation) (Variable index' instanciation')
  | index == index' = do
      Instanciation.unify instanciation instanciation'
unify (Shift evidence) evidence' = do
  evidence' <- unshift evidence'
  unify evidence evidence'
unify evidence (Shift evidence') = do
  evidence <- unshift evidence
  unify evidence evidence'
unify _ _ = error "unify evidence can't fail"

unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
unshift = \case
  Variable variable instanciation -> do
    let fail = error "unshift can't fail"
    instanciation <- Instanciation.unshift instanciation
    pure $ Variable (Shift.map (Shift.Unshift fail) variable) instanciation
  Logical reference -> do
    readSTRef reference >>= \case
      Solved evidence -> unshift evidence
      Unsolved -> do
        box <- newSTRef Unsolved
        writeSTRef reference (Solved $ Shift $ Logical box)
        pure $ Logical box
  Shift evidence -> pure evidence
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
  Shift evidence -> shift <$> solve position evidence
  Logical reference ->
    Solve (readSTRef reference) >>= \case
      Solved evidence -> solve position evidence
      Unsolved -> unsupportedFeatureConstraintedTypeDefaulting position
