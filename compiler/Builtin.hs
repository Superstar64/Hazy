module Builtin (builtinData, builtinClass) where

import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra)
import Core.Tree.Data (Data)
import qualified Core.Tree.Declarations as Declarations (typeExtras, types)
import qualified Core.Tree.Module as Module (simplify)
import qualified Core.Tree.Module as Module.Core
import Core.Tree.TypeDeclaration (TypeDeclaration (..))
import Core.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Core.Tree.TypeDeclarationExtra as TypeDeclarationExtra
import Core.Tree.TypeDefinition (TypeDefinition (..))
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Semantic.Check.Go.Module as Module (check)
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term0 as Term0
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type as Type (Index (..))
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Binding.Constructor as Constructor (Binding, BindingF (..), Detail (..))
import Semantic.Resolve.Binding.Term as Term (Binding, BindingF (..), Detail (..), Selector (..))
import Semantic.Resolve.Binding.Type as Type (Binding, BindingF (..), Detail (..))
import Semantic.Resolve.Bindings (Bindings (..), BindingsF (..))
import qualified Semantic.Resolve.Temporary.Complete.Declarations as Declarations (bindings)
import qualified Semantic.Resolve.Temporary.Complete.Module as Module (resolve, shrink)
import qualified Semantic.Resolve.Temporary.Complete.Module as Module.Resolve
import Semantic.Scope (Global)
import qualified Semantic.Shift as Shift
import qualified Semantic.Tree.Module as Module (connect, seperate)
import Syntax.Extensions (Extensions (..))
import qualified Syntax.Parser as Parser
import Syntax.ParserCombinator (internal)
import qualified Syntax.Tree.Module as Module (parse)

extensions :: Extensions
extensions =
  Extensions
    { implicitPrelude = False,
      stableImports = False,
      unorderedRecords = False,
      constructorFields = False,
      permissiveUpdates = False,
      hygienicHiding = False
    }

abort :: a
abort = error "bad internal"

overrideTerm :: Type2.Index scope -> Term.Binding Global -> Term.Binding scope
overrideTerm typeIndex = \case
  position Term.:@ Identity Term.Binding {index, fixity, selector} ->
    position
      Term.:@ Identity
        Term.Binding
          { index = case index of
              Term2.Select Selector.Index {typeIndex = Type2.Index (Type.Global 0 0), selectorIndex} ->
                Term2.Select Selector.Index {typeIndex = typeIndex, selectorIndex = selectorIndex}
              Term2.Method Method.Index {typeIndex = Type2.Index (Type.Global 0 0), methodIndex} ->
                Term2.Method Method.Index {typeIndex = typeIndex, methodIndex = methodIndex}
              _ -> abort,
            fixity,
            selector = case selector of
              Normal -> Normal
              Selector Selector.Index {typeIndex = Type2.Index (Type.Global 0 0), selectorIndex} ->
                Selector Selector.Index {typeIndex = typeIndex, selectorIndex = selectorIndex}
              _ -> abort
          }

overrideConstructor :: Type2.Index scope -> Constructor.Binding Global -> Constructor.Binding scope
overrideConstructor
  typeIndex = \case
    position
      Constructor.:@ Identity
                       Constructor.Binding
                         { index,
                           fixity,
                           fields,
                           selections,
                           unordered,
                           fielded,
                           single
                         } ->
        position
          Constructor.:@ Identity
            Constructor.Binding
              { index = case index of
                  Constructor.Index {typeIndex = Type2.Index (Type.Global 0 0), constructorIndex} ->
                    Constructor.Index {typeIndex, constructorIndex}
                  _ -> abort,
                fixity,
                fields,
                selections,
                unordered,
                fielded,
                single
              }

overrideType :: Type2.Index scope -> Type.Binding Global -> Type.Binding scope
overrideType
  typeIndex = \case
    header
      Type.:@ Identity
                Type.Binding
                  { index,
                    methods
                  } ->
        header
          Type.:@ Identity
            Type.Binding
              { index = case index of
                  Type3.Index (Type2.Index (Type.Global 0 0)) -> Type3.Index typeIndex
                  _ -> abort,
                methods
              }

overrideBinding :: Type2.Index scope -> Bindings stability Global -> Bindings stability scope
overrideBinding typeIndex Bindings {terms, constructors, types, stability} =
  Bindings
    { terms = overrideTerm typeIndex <$> terms,
      constructors = overrideConstructor typeIndex <$> constructors,
      types = overrideType typeIndex <$> types,
      stability
    }

builtin ::
  Type2.Index scope ->
  Text ->
  ( Bindings () scope,
    TypeDeclaration scope,
    TypeDeclarationExtra scope
  )
builtin typeIndex string =
  (overrideBinding typeIndex bindings, Shift.map category typex, Shift.map category extra)
  where
    category = Shift.Builtin typeIndex
    parsed = runIdentity $ Parser.parse extensions Module.parse internal string
    complete = runIdentity $ Module.resolve $ Vector.singleton parsed
    resolveDeclarations = Module.Resolve.declarations $ Vector.head complete
    bindings = Declarations.bindings (Term0.Global 0) (Type0.Global 0) resolveDeclarations
    resolve = Module.shrink <$> complete
    check = Module.seperate $ Module.check $ Module.connect resolve
    core = Module.simplify <$> check
    coreDeclarations = Module.Core.declarations $ Vector.head core
    typex = Vector.head $ Declarations.types $ coreDeclarations
    extra = Vector.head $ Declarations.typeExtras $ coreDeclarations

builtinData :: Type2.Index scope -> Text -> (Bindings () scope, Data scope)
builtinData typex string = case builtin typex string of
  (bindings, declaration, _) ->
    ( bindings,
      case declaration of
        TypeDeclaration {definition = Data datax} -> datax
        _ -> abort
    )

builtinClass :: Type2.Index scope -> Text -> (Bindings () scope, Class scope, ClassExtra scope)
builtinClass typex string = case builtin typex string of
  (bindings, declaration, extra) ->
    ( bindings,
      case declaration of
        TypeDeclaration {definition = Class classx} -> classx
        _ -> abort,
      case extra of
        TypeDeclarationExtra.Class extra -> extra
        _ -> abort
    )
