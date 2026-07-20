module Semantic.Tree.Body where

import qualified Data.Strict.Vector1 as Strict
import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)
import Semantic.Tree.Statements (Statements)
import qualified Semantic.Tree.Statements as Statements (Guard)

data Body layout stage scope
  = Body {body :: !(Expression layout stage scope)}
  | Guards {guards :: !(Strict.Vector1 (Statements Statements.Guard layout stage scope))}
  deriving (Show)

instance Shift0.Functor (Body layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Body layout stage) where
  map category = \case
    Body expression -> Body (Shift.map category expression)
    Guards statements -> Guards (fmap (Shift.map category) statements)

instance FreeTermVariables (Body layout) where
  freeTermVariables target = \case
    Body expression -> freeTermVariables target expression
    Guards statements -> foldMap (freeTermVariables target) statements

instance Connect Body where
  connect = \case
    Body expression -> Body (connect expression)
    Guards statements -> Guards (fmap connect statements)
  seperate = \case
    Body expression -> Body (seperate expression)
    Guards statements -> Guards (fmap seperate statements)
