module Semantic.Unify.Zonk where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import Semantic.Scope (Environment (..))
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Unify.Type (Logical)

data Zonker s s' where
  Zonker :: Zonker s s

instance Zonk TypeF

class Zonk typef where
  zonk ::
    Zonker s s' ->
    Shift0.Category (scope1 ':+ scopex) scope ->
    typef (Logical s (scope1 ':+ scopex)) scope ->
    ST s (typef (Logical s' scopex) scope)
