module Stage3.Temporary.InstanceMethod where

import Control.Monad.ST (ST)
import Stage2.Scope (Environment (..), Local)
import Stage3.Temporary.Definition (Definition)
import qualified Stage3.Temporary.Definition as Definition
import qualified Stage3.Tree.InstanceMethod as Solved
import qualified Stage3.Unify as Unify
import {-# SOURCE #-} qualified Stage4.Tree.Expression as Simple (Expression)
import qualified Stage4.Tree.SchemeOver as Simple (SchemeOver)

data InstanceMethod s scope
  = Definition
      { definition :: !(Definition s (Local ':+ Local ':+ scope)),
        definition' :: !(Unify.Delay (Simple.SchemeOver Simple.Expression) s (Local ':+ scope))
      }
  | Default
      { definition' :: !(Unify.Delay (Simple.SchemeOver Simple.Expression) s (Local ':+ scope))
      }

instance Unify.Zonk InstanceMethod where
  zonk zonker = \case
    Definition {definition, definition'} -> do
      definition <- Unify.zonk zonker definition
      definition' <- Unify.zonk zonker definition'
      pure Definition {definition, definition'}
    Default {definition'} -> do
      definition' <- Unify.zonk zonker definition'
      pure Default {definition'}

solve :: InstanceMethod s scope -> ST s (Solved.InstanceMethod scope)
solve = \case
  Definition {definition, definition' = Unify.Delay definition'} -> do
    definition <- Definition.solve definition
    definition' <- definition'
    pure Solved.Definition {definition, definition'}
  Default {definition' = Unify.Delay definition'} -> do
    definition' <- definition'
    pure Solved.Default {definition'}
