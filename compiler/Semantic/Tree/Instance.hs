{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Instance where

import Data.Functor.Identity (Identity)
import Semantic.Connect (Connect (..))
import Semantic.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.InstanceDefinition2 (InstanceDefinition2)
import Syntax.Position (Position)

data Instance solve loeb layout stage scope = Instance
  { startPosition :: !Position,
    definition :: !(InstanceDefinition2 solve loeb layout stage scope)
  }
  deriving (Show)

instance (Functor solve, Functor loeb) => Shift0.Functor (Instance solve loeb layout stage) where
  map = Shift.mapDefault

instance (Functor solve, Functor loeb) => Shift.Functor (Instance solve loeb layout stage) where
  map category Instance {startPosition, definition} =
    Instance
      { startPosition,
        definition = Shift.map category definition
      }

instance Traversable2 (Instance solve) where
  traverse2 f Instance {startPosition, definition} =
    instancex <$> traverse2 f definition
    where
      instancex definition = Instance {startPosition, definition}

instance (solve ~ Identity, loeb ~ Identity) => Connect (Instance solve loeb) where
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
