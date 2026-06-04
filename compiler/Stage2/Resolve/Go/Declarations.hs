{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Declarations where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (mapMaybe)
import Stage1.Extensions (Extensions (Extensions, stableImports))
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (toImport)
import qualified Stage1.Tree.Declarations as Stage1 (Declarations (..))
import qualified Stage2.Index.Term as Index.Term
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type0 as Type0
import Stage2.Layout (Normal)
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Resolve.Context as Context
import Stage2.Resolve.Import (StableImports (StableImports), pickImports)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (shift)
import Stage2.Stage (Resolve)
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Declarations as Complete
import Stage2.Tree.Declarations (Declarations (..))

resolve ::
  Context scope ->
  Stage1.Declarations Position ->
  ( Context (Scope.Declaration ':+ scope),
    Declarations locality Normal Resolve (Scope.Declaration ':+ scope)
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
        bindings <- Complete.bindings Term0.Declaration Type0.Declaration complete,
        context <- context {locals = bindings Bindings.</> locals} =
          context
    complete = runIdentity $ Complete.resolve context extensions Index.Term.Declaration (toList declarations)
