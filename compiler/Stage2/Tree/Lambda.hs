{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Lambda where

import Stage1.Position (Position)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage2.Tree.Expression (Expression)
import Stage2.Tree.Pattern (Pattern)

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

instance Shift (Lambda layout stage) where
  shift = shiftDefault

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
