module Semantic.Resolve.Import
  ( Module (..),
    pickPrelude,
    pickImports,
    pickModules,
  )
where

import Control.Applicative (liftA)
import Control.Monad (liftM2)
import Data.Foldable (fold, toList)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Set as Set
import Error
  ( constructorNotInScope,
    cyclicalImports,
    fieldNotInScope,
    moduleNotFound,
    moduleNotInScope,
    typeNotInScope,
  )
import Graph.Topological (Loeb (..), Loeb1 (..), loeb, loeb1)
import qualified Graph.Topological as Graph
import Graph.Topological1 (Formula1 (..))
import qualified Semantic.Resolve.Binding.Constructor as Constructor
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Semantic.Resolve.Bindings (Bindings, BindingsF (..), (!-), (!=), (!=.))
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Builtin (builtin)
import Semantic.Resolve.Canonical (Canonical (..), CanonicalF (..), (!))
import Semantic.Resolve.Core (Core (..), CoreF)
import qualified Semantic.Resolve.Core as Core
import Semantic.Resolve.Functor2 (Proper (..), Traversable2, fmap2)
import Semantic.Resolve.Stability (Stability (..))
import Semantic.Scope (Environment)
import qualified Semantic.Scope as Scope
import Syntax.Extensions (Extensions (Extensions, implicitPrelude, stableImports))
import qualified Syntax.Extensions as Syntax (Extensions (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Alias as Syntax (Alias (Alias, NoAlias, name))
import qualified Syntax.Tree.ExportSymbol as Syntax.Export
import qualified Syntax.Tree.Exports as Syntax (Exports (..))
import qualified Syntax.Tree.Import as Syntax (Import (..))
import Syntax.Tree.ImportFields (Fields (..))
import qualified Syntax.Tree.ImportFields as Syntax (Fields (AllFields, Fields))
import qualified Syntax.Tree.ImportSymbol as Syntax.Import
import qualified Syntax.Tree.ImportSymbols as Syntax (Symbols (..))
import Syntax.Tree.Marked (Marked (..))
import Syntax.Tree.Qualification (Qualification (..))
import Syntax.Variable
  ( Constructor (ConstructorIdentifier),
    ConstructorIdentifier,
    FullQualifiers ((:..)),
    Name (Constructor, Variable),
    QualifiedConstructorIdentifier ((:=.)),
    QualifiedVariable (..),
    Qualifiers (Local, (:.)),
    Variable,
    prelude,
    prelude',
  )

type Formula :: ((Type -> Type) -> Environment -> Type) -> Environment -> Type -> Type
data Formula bindings scope a = Formula
  { position :: [Position],
    run :: forall loeb. (Monad loeb) => bindings loeb scope -> loeb a
  }

instance Functor (Formula bindings scope) where
  fmap = liftA

instance Applicative (Formula bindings scope) where
  pure a = Formula {position = [], run = const $ pure a}
  Formula {position = position1, run = function} <*> Formula {position = position2, run = argument} =
    Formula
      { position = position1 ++ position2,
        run = \complete -> function complete <*> (argument complete)
      }

proper :: Formula bindings scope a -> Graph.Formula (Proper bindings scope) s a
proper Formula {position, run} =
  Graph.Formula
    { -- todo merge error messages for cycles
      cycle = case position of
        [] -> undefined
        position : _ -> cyclicalImports position,
      run = \(Proper proper) -> run proper
    }

type Morph2 ::
  ((Type -> Type) -> Environment -> Type) ->
  ((Type -> Type) -> Environment -> Type) ->
  Environment ->
  Type
newtype Morph2 f g scope = Morph2 (forall m. (Monad m) => f m scope -> g m scope)

contra :: Morph2 f1 f2 scope -> NaturalTransformation (Formula f2 scope) (Formula f1 scope)
contra (Morph2 f) = Morph (\Formula {position, run} -> Formula {position, run = run . f})

contramap :: (Traversable2 f) => Morph2 f1 f2 scope -> f (Formula f2 scope) scope -> f (Formula f1 scope) scope
contramap = fmap2 . contra

strict ::
  (Monad loeb) =>
  ( Map FullQualifiers (Identity (BindingsF () loeb scope)) ->
    Map Qualifiers (Identity (BindingsF Stability (Formula CanonicalF scope) scope))
  ) ->
  CanonicalF loeb scope ->
  CoreF loeb scope
strict pick' canonical =
  Core.fromMap
    $ fmap (fmap2 $ Morph $ \Formula {run} -> run canonical)
    $ fmap runIdentity
      . pick'
      . fmap Identity
    $ runCanonical canonical

selectTerm ::
  Variable ->
  Term.BindingF loeb' scope' ->
  Term.BindingF (Formula (BindingsF ()) scope) scope
selectTerm name (position Term.:@ _) =
  position
    Term.:@ Formula
      { position = [position],
        run = \bindings -> Term.value $ bindings !- position :@ name
      }

selectConstructor ::
  Constructor ->
  Constructor.BindingF loeb' scope' ->
  Constructor.BindingF (Formula (BindingsF ()) scope) scope
selectConstructor name (position Constructor.:@ _) =
  position
    Constructor.:@ Formula
      { position = [position],
        run = \bindings -> Constructor.value $ bindings != position :@ name
      }

selectType ::
  ConstructorIdentifier ->
  Type.BindingF loeb' scope' ->
  Type.BindingF (Formula (BindingsF ()) scope) scope
selectType name (header@Type.Header {position} Type.:@ _) =
  header
    Type.:@ Formula
      { position = [position],
        run = \bindings -> Type.value $ bindings !=. position :@ name
      }

pickTerm ::
  (Monad loeb) =>
  Position ->
  Variable ->
  loeb (BindingsF () (Formula (BindingsF ()) scope) scope)
pickTerm position name =
  pure
    Bindings
      { terms = Map.singleton name (selectTerm name value),
        constructors = Map.empty,
        types = Map.empty,
        stability = ()
      }
  where
    value = position Term.:@ Const ()

pickData ::
  (Monad loeb) =>
  Position ->
  ConstructorIdentifier ->
  Fields ->
  loeb (BindingsF stability loeb' scope') ->
  loeb (BindingsF () (Formula (BindingsF ()) scope) scope)
pickData position name AllFields request = do
  Bindings {terms, constructors, types} <- request
  let term name =
        selectTerm name (Term.position (terms Map.! name) Term.:@ Const ())

      constructor name =
        selectConstructor name (Constructor.position (constructors Map.! name) Constructor.:@ Const ())
      typex = selectType name (header Type.:@ Const ())
        where
          header =
            Type.Header
              { position,
                fields = Type.fields $ Type.header $ types Map.! name,
                constructors = Type.constructors $ Type.header $ types Map.! name
              }
  case Map.lookup name types of
    Just (Type.Header {fields, constructors} Type.:@ _) -> do
      pure $
        Bindings
          { terms = Map.fromSet term fields,
            constructors = Map.fromSet constructor constructors,
            types = Map.singleton name typex,
            stability = ()
          }
    Nothing -> typeNotInScope position
pickData position name Fields {picks} _ =
  pure
    Bindings
      { terms = Map.mapWithKey term fields,
        constructors = Map.mapWithKey constructor constructors,
        types,
        stability = ()
      }
  where
    forceType :: (Monad m) => BindingsF stable m scope -> m ()
    forceType bindings = do
      _ <- Type.value $ bindings !=. position :@ name
      pure ()
    constructors =
      Map.fromList [(name, position) | position :@ Constructor name <- toList picks]
    fields =
      Map.fromList [(name, position) | position :@ Variable name <- toList picks]

    term name position =
      position
        Term.:@ Formula
          { position = [position],
            run = \bindings -> do
              forceType bindings
              Term.value $ bindings !- position :@ name
          }
    constructor name position =
      position
        Constructor.:@ Formula
          { position = [position],
            run = \bindings -> do
              forceType bindings
              Constructor.value $ bindings != position :@ name
          }
    types = Map.singleton name typex
      where
        typex =
          Type.Header
            { position,
              constructors = Map.keysSet constructors,
              fields = Map.keysSet fields
            }
            Type.:@ Formula
              { position = [position],
                run = \bindings ->
                  let checkSelector name position ()
                        | Set.member name realSelectors = ()
                        | otherwise = fieldNotInScope position name
                      checkConstructor name position ()
                        | Set.member name realConstructors = ()
                        | otherwise = constructorNotInScope position name
                      Type.Header
                        { fields = realSelectors,
                          constructors = realConstructors
                        }
                        Type.:@ value = bindings !=. position :@ name
                   in if
                        | () <- Map.foldrWithKey checkSelector () fields,
                          () <- Map.foldrWithKey checkConstructor () constructors ->
                            value
              }

pickSymbol ::
  (Monad loeb) =>
  Syntax.Import.Symbol ->
  loeb (BindingsF stability loeb' scope') ->
  loeb (BindingsF () (Formula (BindingsF ()) scope) scope)
pickSymbol Syntax.Import.Definition {variable = startPosition :@ variable} _ =
  pickTerm startPosition variable
pickSymbol
  Syntax.Import.Data
    { typeVariable = startPosition :@ typeVariable,
      fields
    }
  request =
    pickData startPosition typeVariable fields request

pickPrelude' ::
  (Monad loeb) =>
  Position ->
  [Syntax.Import Position] ->
  Map FullQualifiers (loeb (BindingsF stable' loeb' scope')) ->
  Map Qualifiers (loeb (BindingsF Stability (Formula (CanonicalF) scope) scope))
pickPrelude' position declarations request = case not $ any isPrelude declarations of
  True
    | Just request <- Map.lookup prelude request ->
        let binding = do
              Bindings {terms, constructors, types} <- request
              let bindings =
                    Bindings
                      { terms = Map.mapWithKey selectTerm terms,
                        constructors = Map.mapWithKey selectConstructor constructors,
                        types = Map.mapWithKey selectType types,
                        stability = Stable [position]
                      }
              pure $ contramap (Morph2 (! position :@ prelude)) bindings
         in Map.fromList [(Local, binding), (prelude', binding)]
    | otherwise -> error "no prelude"
  False -> Map.empty
  where
    isPrelude :: Syntax.Import Position -> Bool
    isPrelude (Syntax.Import {target})
      | target == prelude = True
    isPrelude _ = False

pickImports' ::
  (Monad loeb) =>
  Extensions ->
  [Syntax.Import Position] ->
  Map FullQualifiers (loeb (BindingsF stable' loeb' scope')) ->
  Map Qualifiers (loeb (BindingsF Stability (Formula CanonicalF scope) scope))
pickImports' Extensions {stableImports, hygienicHiding} declarations request =
  Map.fromListWith (liftM2 (<>)) $ foldMap selections declarations
  where
    selections Syntax.Builtin {targetPosition, target} =
      [(Local, bindings), (name, bindings)]
      where
        update = Bindings.updateStability (Stable [targetPosition])
        poly = fmap2 (Morph $ pure . runIdentity)
        bindings = pure $ update $ poly builtin
        name
          | root :.. name <- target = root :. name
    selections Syntax.Import {qualification, targetPosition = position, target = name, alias, symbols} =
      (qualifiedName, bindings) : case qualification of
        Qualified -> []
        Unqualified -> [(Local, bindings)]
      where
        qualifiedName = case alias of
          Syntax.NoAlias | root :.. name <- name -> root :. name
          Syntax.Alias {name} | root :.. name <- name -> root :. name
        base = case Map.lookup name request of
          Just bindings -> Bindings.updateStability stability <$> bindings
          Nothing -> moduleNotFound position
          where
            stability
              | name == prelude = Stable [position]
              | not stableImports = Stable [position]
              | otherwise = Unstable position
        all = do
          Bindings {terms, constructors, types, stability} <- base
          pure
            Bindings
              { terms = Map.mapWithKey selectTerm terms,
                constructors = Map.mapWithKey selectConstructor constructors,
                types = Map.mapWithKey selectType types,
                stability
              }
        bindings = case symbols of
          Syntax.Symbols {symbols} -> update <$> foldr combine empty items
            where
              combine = liftM2 (<>)
              empty = pure $ mempty
              items = [contramap (Morph2 (! position :@ name)) <$> pickSymbol symbol base | symbol <- toList symbols]
              update = Bindings.updateStability (Stable [position])
          Syntax.All -> contramap (Morph2 (! position :@ name)) <$> all
          Syntax.Hiding {symbols} -> do
            base@Bindings {terms, constructors, types, stability} <- all
            let bindings =
                  Bindings
                    { terms = foldr Map.delete terms termDeletions,
                      constructors = foldr Map.delete constructors (constructorDeletions ++ unhygienic),
                      types = foldr Map.delete types typeDeletions,
                      stability
                    }
                termDeletions = do
                  symbol <- toList symbols
                  case symbol of
                    Syntax.Import.Definition {variable = _ :@ variable} -> [variable]
                    Syntax.Import.Data {typeVariable, fields} ->
                      case fields of
                        Syntax.AllFields -> case base !=. typeVariable of
                          Type.Header {fields} Type.:@ _ -> toList fields
                        Syntax.Fields {picks} -> do
                          _ :@ Variable name <- toList picks
                          pure name
                unhygienic
                  | hygienicHiding = []
                  | otherwise = do
                      Syntax.Import.Data {typeVariable = _ :@ typeVariable} <- toList symbols
                      pure $ ConstructorIdentifier typeVariable
                constructorDeletions = do
                  Syntax.Import.Data {typeVariable, fields} <-
                    toList symbols
                  case fields of
                    Syntax.AllFields -> case base !=. typeVariable of
                      Type.Header {constructors} Type.:@ _ -> toList constructors
                    Syntax.Fields {picks} -> do
                      _ :@ Constructor name <- toList picks
                      pure name
                typeDeletions = do
                  Syntax.Import.Data {typeVariable = _ :@ typeVariable} <- toList symbols
                  pure typeVariable
            pure $ contramap (Morph2 (! position :@ name)) $ bindings

pickExports ::
  (Monad loeb) =>
  BindingsF () Identity scope ->
  Syntax.Exports ->
  Map Qualifiers (loeb (BindingsF Stability scope' loeb')) ->
  loeb (BindingsF () (Formula CoreF scope) scope)
pickExports _ Syntax.Exports {exports} request = do
  let pick export = case export of
        Syntax.Export.Module {modulex = position :@ root :.. name} ->
          case Map.lookup (root :. name) request of
            Just request -> do
              Bindings {terms, constructors, types, stability} <- request
              let bindings =
                    Bindings
                      { terms = Map.mapWithKey selectTerm terms,
                        constructors = Map.mapWithKey selectConstructor constructors,
                        types = Map.mapWithKey selectType types,
                        stability
                      }
              pure $
                contramap (Morph2 (Bindings.updateStability () . (Core.! position :@ root :. name))) $
                  bindings
            Nothing -> moduleNotInScope position
        Syntax.Export.Definition {variable = position :@ root :- name} -> do
          bindings <- pickTerm position name
          pure $
            contramap (Morph2 (Bindings.updateStability () . (Core.! position :@ root))) $
              Bindings.updateStability (Stable [position]) bindings
        Syntax.Export.Data {typeVariable = position :@ root :=. name, fields} -> do
          let base = case Map.lookup root request of
                Just base -> base
                Nothing -> moduleNotInScope position
          bindings <- pickData position name fields base
          pure $
            contramap (Morph2 (Bindings.updateStability () . (Core.! position :@ root))) $
              Bindings.updateStability (Stable [position]) bindings
  bindings <- traverse pick exports
  pure $ Bindings.updateStability () $ fold bindings
pickExports defaultx Syntax.Default _ = pure $ poly defaultx
  where
    poly = fmap2 (Morph $ pure . runIdentity)

data Module = Module
  { modulePosition :: Position,
    extensions :: Syntax.Extensions,
    imports :: [Syntax.Import Position],
    exports :: Syntax.Exports,
    base :: Bindings () Scope.Global
  }

pickModule ::
  FullQualifiers ->
  Module ->
  Formula1 (Map FullQualifiers) s (BindingsF () (Graph.Formula (Proper CanonicalF Scope.Global) s') Scope.Global)
pickModule (root :.. name) Module {modulePosition, extensions, exports, imports, base} =
  Formula1
    { cycle = cyclicalImports modulePosition,
      run = \modules -> do
        let regular = base
            update = Bindings.updateStability (Stable [modulePosition])
        base <- pure $ update base
        let pick ::
              (Monad m) =>
              Map FullQualifiers (m (BindingsF stability m' Scope.Global)) ->
              Map Qualifiers (m (BindingsF Stability (Formula CanonicalF Scope.Global) Scope.Global))
            pick request =
              let base = pickImports' extensions imports request
                  prelude =
                    if implicitPrelude
                      then pickPrelude' modulePosition imports request
                      else Map.empty
                  combined = Map.unionWith (liftM2 (<>)) base prelude
                  shadower = pure <$> shadow
               in Map.unionWith (liftM2 Bindings.prefer) shadower combined
            shadow =
              let morph = Morph $ pure . runIdentity
               in Map.fromList [(Local, fmap2 morph base), (root :. name, fmap2 morph base)]
        let imports = pick modules
        exports <- pickExports regular exports imports
        pure $ fmap2 (Morph proper) $ contramap (Morph2 $ strict pick) exports
    }
  where
    Extensions {implicitPrelude} = extensions

pickPrelude ::
  Position ->
  [Syntax.Import Position] ->
  Canonical scope ->
  Core scope
pickPrelude position declarations = strict (pickPrelude' position declarations)

pickImports ::
  Extensions ->
  [Syntax.Import Position] ->
  Canonical scope ->
  Core scope
pickImports extensions declarations = strict (pickImports' extensions declarations)

pickModules :: Map FullQualifiers Module -> Canonical Scope.Global
pickModules modules =
  runProper $
    loeb $
      Loeb $
        Proper $
          Canonical $
            loeb1 $
              Loeb1 $
                Map.mapWithKey pickModule modules
