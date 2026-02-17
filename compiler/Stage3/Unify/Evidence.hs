module Stage3.Unify.Evidence where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (ambiguousType)
import Stage1.Position (Position)
import Stage2.Scope (Environment (..))
import Stage2.Shift (shift)
import qualified Stage2.Shift as Shift
import qualified Stage3.Index.Evidence as Evidence
import Stage3.Unify.Class (Zonk (..), Zonker (..))
import qualified Stage4.Tree.Evidence as Simple (Evidence (..))

data Evidence s scope where
  Variable :: !(Evidence.Index scope) -> Evidence s scope
  Call :: !(Evidence s scope) -> !(Strict.Vector (Evidence s scope)) -> Evidence s scope
  Super :: !(Evidence s scope) -> !Int -> Evidence s scope
  Logical :: !(STRef s (Box s scope)) -> Evidence s scope
  Shift :: !(Evidence s scopes) -> Evidence s (scope ':+ scopes)

instance Zonk Evidence where
  zonk Zonker = \case
    Variable variable -> pure $ Variable variable
    Call function arguments -> do
      function <- zonk Zonker function
      arguments <- traverse (zonk Zonker) arguments
      pure $ Call function arguments
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
    combine Unsolved evidence' = writeSTRef reference evidence'
    combine evidence Unsolved = writeSTRef reference' evidence
unify (Logical reference) evidence' =
  readSTRef reference >>= \case
    Solved evidence -> unify evidence evidence'
    Unsolved -> writeSTRef reference (Solved evidence')
unify evidence (Logical reference') =
  readSTRef reference' >>= \case
    Solved evidence' -> unify evidence evidence'
    Unsolved -> writeSTRef reference' (Solved evidence)
unify (Variable index) (Variable index')
  | index == index' = pure ()
unify (Call function arguments) (Call function' arguments') = do
  unify function function'
  sequence_ $ Strict.Vector.zipWith unify arguments arguments'
unify (Shift evidence) evidence' = do
  evidence' <- unshift evidence'
  unify evidence evidence'
unify evidence (Shift evidence') = do
  evidence <- unshift evidence
  unify evidence evidence'
unify _ _ = error "unify evidence can't fail"

unshift :: Evidence s (scope ':+ scopes) -> ST s (Evidence s scopes)
unshift = \case
  Variable variable -> do
    let fail = error "unshift can't fail"
    pure $ Variable (Shift.map (Shift.Unshift fail) variable)
  Call function arguments -> do
    function <- unshift function
    arguments <- traverse unshift arguments
    pure $ Call function arguments
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

solve :: Position -> Evidence s scope -> ST s (Simple.Evidence scope)
solve position = \case
  Variable variable -> pure Simple.Variable {variable}
  Call function arguments -> do
    function <- solve position function
    arguments <- traverse (solve position) arguments
    pure Simple.Call {function, arguments}
  Super base index -> do
    base <- solve position base
    pure $ Simple.Super {base, index}
  Shift evidence -> shift <$> solve position evidence
  Logical reference ->
    readSTRef reference >>= \case
      Solved evidence -> solve position evidence
      Unsolved -> ambiguousType position
