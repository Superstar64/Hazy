{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Entry where

import Stage1.Position (Position)
import qualified Stage1.Tree.Entry as Stage1 (Entry (..))
import Stage2.Resolve.Context (Context)
import qualified Stage2.Resolve.Go.Scheme as Scheme
import qualified Stage2.Resolve.Go.Type as Type
import Stage2.Stage (Resolve)
import Stage2.Tree.Entry (Entry (..), Restricted (..))
import Stage2.Tree.StrictnessAnnotation (StrictnessAnnotation (..))

resolve :: Context scope -> Stage1.Entry -> Entry Position Resolve scope
resolve context = \case
  Stage1.Lazy {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict = Lazy
      }
  Stage1.Strict {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict = Strict
      }
  Stage1.Polymorphic {startPosition, levity, entry} ->
    Entry
      { startPosition,
        entry = Canonical $ Scheme.resolve context entry,
        strict =
          Polymorphic
            { levity = Type.resolve context levity
            }
      }
