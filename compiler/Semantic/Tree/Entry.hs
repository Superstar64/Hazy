{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Entry where

import Semantic.FreeVariables (FreeTypeVariables (..))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, IsResolve, Resolve)
import Semantic.Tree.Scheme (Scheme)
import qualified Semantic.Tree.Scheme as Scheme
import Semantic.Tree.StrictnessAnnotation (StrictnessAnnotation (..))
import qualified Semantic.Tree.StrictnessAnnotation as StrictnessAnnotation
import Semantic.Tree.Type (Type)
import qualified Semantic.Tree.Type as Type

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

instance FreeTypeVariables (Entry position) where
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

instance (Eq position, IsResolve stage) => Eq (Restricted position stage scope) where
  Canonical scheme1 == Canonical scheme2 = scheme1 == scheme2
  Restricted type1 == Restricted type2 = type1 == type2

instance Shift (Restricted position stage) where
  shift = shiftDefault

instance Shift.Functor (Restricted position stage) where
  map category = \case
    Canonical scheme -> Canonical (Shift.map category scheme)
    Restricted typex -> Restricted (Shift.map category typex)

instance FreeTypeVariables (Restricted position) where
  freeTypeVariables target = \case
    Canonical scheme -> freeTypeVariables target scheme

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
