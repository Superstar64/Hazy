{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Entry where

import Semantic.Resolve.Context (Context)
import qualified Semantic.Resolve.Go.Scheme as Scheme
import qualified Semantic.Resolve.Go.Type as Type
import Semantic.Stage (Resolve)
import Semantic.Tree.Entry (Entry (..), Restricted (..))
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Entry as Syntax (Entry (..))

resolve :: Context scope -> Syntax.Entry -> Entry Position Resolve scope
resolve context = \case
  Syntax.Lazy {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict = Lazy
      }
  Syntax.Strict {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict = Strict
      }
  Syntax.Polymorphic {startPosition, levity, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict =
          Polymorphic
            { levity = Type.resolve context levity
            }
      }
