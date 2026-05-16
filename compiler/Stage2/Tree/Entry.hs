{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Entry where

import Stage1.Position (Position)
import qualified Stage1.Tree.Entry as Stage1 (Entry (..))
import Stage2.FreeVariables (FreeTypeVariables (..))
import Stage2.Resolve.Context (Context)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Exclusive, Resolve)
import Stage2.Tree.Scheme (Scheme)
import qualified Stage2.Tree.Scheme as Scheme
import Stage2.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import qualified Stage2.Tree.StrictnessAnnotation as StrictnessAnnotation
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type

data Entry position stage scope = Entry
  { startPosition :: !position,
    entry :: !(Restricted position stage scope),
    strict :: !(StrictnessAnnotation position stage scope)
  }
  deriving (Show, Eq)

instance Shift (Entry position stage) where
  shift = shiftDefault

instance Shift.Functor (Entry position stage) where
  map category Entry {startPosition, entry, strict} =
    Entry
      { startPosition,
        entry = Shift.map category entry,
        strict = Shift.map category strict
      }

instance FreeTypeVariables (Entry position stage) where
  freeTypeVariables target Entry {entry, strict} =
    concat
      [ freeTypeVariables target entry,
        freeTypeVariables target strict
      ]

-- |
-- This type is here as a temporary measure until polymorphic components are
-- supported
data Restricted position stage scope where
  Canonical :: !(Scheme position Resolve scope) -> Restricted position Resolve scope
  Restricted :: !(Type position Check scope) -> Restricted position Check scope

instance (Show position) => Show (Restricted position stage scope) where
  showsPrec d = \case
    Canonical scheme -> showParen (d > 10) $ showString "Canonical " . showsPrec 11 scheme
    Restricted typex -> showParen (d > 10) $ showString "Restricted " . showsPrec 11 typex

instance (Eq position, Exclusive stage) => Eq (Restricted position stage scope) where
  Canonical scheme1 == Canonical scheme2 = scheme1 == scheme2
  Restricted type1 == Restricted type2 = type1 == type2

instance Shift (Restricted position stage) where
  shift = shiftDefault

instance Shift.Functor (Restricted position stage) where
  map category = \case
    Canonical scheme -> Canonical (Shift.map category scheme)
    Restricted typex -> Restricted (Shift.map category typex)

instance FreeTypeVariables (Restricted position stage) where
  freeTypeVariables target = \case
    Canonical scheme -> freeTypeVariables target scheme
    Restricted typex -> freeTypeVariables target typex

anonymize :: Entry position stage scope -> Entry () stage scope
anonymize Entry {entry, strict} =
  Entry
    { startPosition = (),
      entry = anonymizeRestrict entry,
      strict = StrictnessAnnotation.anonymize strict
    }
  where
    anonymizeRestrict :: Restricted position stage scope -> Restricted () stage scope
    anonymizeRestrict = \case
      Canonical scheme -> Canonical (Scheme.anonymize scheme)
      Restricted typex -> Restricted (Type.anonymize typex)

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
