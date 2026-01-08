{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Module
  ( Module (..),
    labelContext,
    resolve,
  )
where

import Data.Vector (Vector)
import Error (Position)
import qualified Stage1.Tree.Module as Stage1 (Module)
import Stage1.Variable (FullQualifiers, toQualifiers)
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Term as Table.Term
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Label.Context as Label (Context (Context))
import qualified Stage2.Label.Context as Label.Context
import Stage2.Scope (Global)
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Module as Complete
import Stage2.Tree.Declarations (Declarations (Declarations))
import qualified Stage2.Tree.Declarations as Declarations (Declarations (terms, types))
import qualified Stage2.Tree.TermDeclaration as TermDeclaration (labelBinding)
import qualified Stage2.Tree.TypeDeclaration as TypeDeclaration
import Verbose (Debug)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

labelContext :: Vector Module -> Label.Context Global
labelContext modules =
  Label.Context
    { terms = Table.Term.Global $ labelTerms modules,
      locals = Local.Table.Global,
      types = Table.Type.Global $ labelTypes modules
    }
  where
    labelTerms = fmap $ \Module {name, declarations = Declarations {terms}} ->
      TermDeclaration.labelBinding (toQualifiers name) <$> terms
    labelTypes = fmap $ \Module {name, declarations = Declarations {types}} ->
      TypeDeclaration.labelBinding (toQualifiers name) <$> types

resolve :: (Debug verbose) => Vector (Stage1.Module Position) -> verbose (Vector Module)
resolve modules = fmap Complete.shrink <$> Complete.resolve modules
