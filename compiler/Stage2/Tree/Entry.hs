{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Entry where

import Stage1.Position (Position)
import qualified Stage1.Tree.Entry as Stage1 (Entry (..))
import Stage2.Resolve.Context (Context)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme

data Entry position scope = Entry
  { startPosition :: !position,
    entry :: !(Scheme position scope),
    strict :: !Bool
  }
  deriving (Show, Eq)

instance Shift (Entry position) where
  shift = shiftDefault

instance Shift.Functor (Entry position) where
  map category Entry {startPosition, entry, strict} =
    Entry
      { startPosition,
        entry = Shift.map category entry,
        strict
      }

anonymize :: Entry position scope -> Entry () scope
anonymize Entry {entry, strict} =
  Entry
    { startPosition = (),
      entry = Scheme.anonymize entry,
      strict
    }

resolve :: Context scope -> Stage1.Entry -> Entry Position scope
resolve context = \case
  Stage1.Lazy {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Scheme.resolve context entry,
        strict = False
      }
  Stage1.Strict {startPosition, entry} ->
    Entry
      { startPosition,
        entry = Scheme.resolve context entry,
        strict = True
      }
