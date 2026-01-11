{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Declarations where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Stage1.Extensions (Extensions (Extensions, stableImports))
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (toImport)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations (..))
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type as Type (Index (..))
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Resolve.Context as Context
import Stage2.Resolve.Import (StableImports (StableImports), pickImports)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Declarations as Complete
import Stage2.Tree.Instance (Instance)
import Stage2.Tree.Shared (Shared)
import Stage2.Tree.TermDeclaration (TermDeclaration)
import Stage2.Tree.TypeDeclaration (TypeDeclaration)

data Declarations scope = Declarations
  { terms :: !(Vector (TermDeclaration scope)),
    types :: !(Vector (TypeDeclaration scope)),
    shared :: !(Vector (Shared scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }
  deriving (Show)

instance Shift Declarations where
  shift = shiftDefault

instance Shift.Functor Declarations where
  map
    category
    Declarations
      { terms,
        types,
        shared,
        dataInstances,
        classInstances
      } =
      Declarations
        { terms = fmap (Shift.map category) terms,
          types = fmap (Shift.map category) types,
          shared = fmap (Shift.map category) shared,
          dataInstances = fmap (Shift.mapmap category . fmap (Shift.map category)) dataInstances,
          classInstances = fmap (Shift.mapmap category . fmap (Shift.map category)) classInstances
        }

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Scope.Declaration ':+ scope),
    Declarations (Scope.Declaration ':+ scope)
  )
resolve initial@Context {canonical, extensions} Stage1.Declarations {declarations} =
  (context, Complete.shrink complete)
  where
    context
      | context <- initial,
        Extensions {stableImports} <- extensions,
        imports <- pickImports (StableImports stableImports) (mapMaybe Stage1.toImport (toList declarations)) canonical,
        context <- imports Context.</> context,
        context@Context {locals} <- shift context,
        bindings <- Complete.bindings Term0.Declaration Type.Declaration complete,
        context <- context {locals = bindings Bindings.</> locals} =
          context
    complete = runIdentity $ Complete.resolve context extensions (toList declarations)
