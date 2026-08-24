{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Instance where

import Data.Functor.Identity (Identity)
import Semantic.Connect (Connect (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2)
import Syntax.Position (Position)

data Instance solve layout stage scope = Instance
  { startPosition :: !Position,
    definition :: !(InstanceDefinition2 solve layout stage scope)
  }
  deriving (Show)

instance (Functor solve) => Shift0.Functor (Instance solve layout stage) where
  map = Shift.mapDefault

instance (Functor solve) => Shift.Functor (Instance solve layout stage) where
  map category Instance {startPosition, definition} =
    Instance
      { startPosition,
        definition = Shift.map category definition
      }

instance (solve ~ Identity) => Connect (Instance solve) where
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
