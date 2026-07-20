{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Lambda where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeTermVariables
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope (Pattern)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import {-# SOURCE #-} Semantic.Tree.Expression (Expression)
import Semantic.Tree.Pattern (Pattern)
import Syntax.Position (Position)

data Lambda layout stage scope
  = Plain
      { plain :: !(Expression layout stage scope)
      }
  | Bound
      { boundPosition :: !Position,
        parameter :: !(Pattern stage scope),
        body :: !(Lambda layout stage (Scope.Pattern ':+ scope))
      }
  deriving (Show)

instance Shift0.Functor (Lambda layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Lambda layout stage) where
  map category = \case
    Plain {plain} -> Plain {plain = Shift.map category plain}
    Bound {boundPosition, parameter, body} ->
      Bound
        { boundPosition,
          parameter = Shift.map category parameter,
          body = Shift.map (Shift.Over category) body
        }

instance FreeTermVariables (Lambda layout) where
  freeTermVariables target = \case
    Plain {plain} -> freeTermVariables target plain
    Bound {body} -> freeTermVariables (FreeTermVariables.Over target) body

instance Connect Lambda where
  connect = \case
    Plain {plain} ->
      Plain
        { plain = connect plain
        }
    Bound {boundPosition, parameter, body} ->
      Bound
        { boundPosition,
          parameter,
          body = connect body
        }
  seperate = \case
    Plain {plain} ->
      Plain
        { plain = seperate plain
        }
    Bound {boundPosition, parameter, body} ->
      Bound
        { boundPosition,
          parameter,
          body = seperate body
        }
