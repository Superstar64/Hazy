{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Instance where

import Semantic.Connect (Connect (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2)
import Syntax.Position (Position)

data Instance layout stage scope = Instance
  { startPosition :: !Position,
    definition :: !(InstanceDefinition2 layout stage scope)
  }
  deriving (Show)

instance Shift0.Functor (Instance layout stage) where
  map = Shift.mapDefault

instance Shift.Functor (Instance layout stage) where
  map category Instance {startPosition, definition} =
    Instance
      { startPosition,
        definition = Shift.map category definition
      }

instance Connect Instance where
  connect Instance {startPosition, definition} =
    Instance
      { startPosition,
        definition = connect definition
      }
  seperate Instance {startPosition, definition} =
    Instance
      { startPosition,
        definition = seperate definition
      }
