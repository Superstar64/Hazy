{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Declarations where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (mapMaybe)
import qualified Semantic.Index.Term as Index.Term
import qualified Semantic.Index.Term0 as Term0 (Index (..))
import qualified Semantic.Index.Type0 as Type0
import Semantic.Layout (Normal)
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Context (Context (..))
import qualified Semantic.Resolve.Context as Context
import Semantic.Resolve.Import (StableImports (StableImports), pickImports)
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.Complete.Declarations as Complete
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (shift)
import Semantic.Stage (Resolve)
import Semantic.Tree.Declarations (Declarations (..))
import Syntax.Extensions (Extensions (Extensions, stableImports))
import Syntax.Position (Position)
import qualified Syntax.Tree.Declaration as Syntax (toImport)
import qualified Syntax.Tree.Declarations as Syntax (Declarations (..))

resolve ::
  Context scope ->
  Syntax.Declarations Position ->
  ( Context (Scope.Declaration ':+ scope),
    Declarations locality Normal Resolve (Scope.Declaration ':+ scope)
  )
resolve initial@Context {canonical, extensions} Syntax.Declarations {declarations} =
  (context, Complete.shrink complete)
  where
    context
      | context <- initial,
        Extensions {stableImports} <- extensions,
        imports <- pickImports (StableImports stableImports) (mapMaybe Syntax.toImport (toList declarations)) canonical,
        context <- imports Context.</> context,
        context@Context {locals} <- shift context,
        bindings <- Complete.bindings Term0.Declaration Type0.Declaration complete,
        context <- context {locals = bindings Bindings.</> locals} =
          context
    complete = runIdentity $ Complete.resolve context extensions Index.Term.Declaration (toList declarations)
