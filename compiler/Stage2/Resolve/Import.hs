module Stage2.Resolve.Import
  ( Module (..),
    StableImports (..),
    pickPrelude,
    pickImports,
    pickModules,
  )
where

import Control.Monad (liftM2)
import Data.Acyclic (Loeb (..), Loeb3 (..), loeb, loeb3)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Trifunctor (Trifunctor (trimap))
import Data.Void (Void)
import Error
  ( constructorNotInScope,
    cyclicalImports,
    fieldNotInScope,
    moduleNotFound,
    moduleNotInScope,
    typeNotInScope,
  )
import Stage1.Extensions (Extensions (Extensions, implicitPrelude, stableImports))
import qualified Stage1.Extensions as Stage1 (Extensions (..))
import Stage1.Position (Position)
import qualified Stage1.Tree.Alias as Stage1 (Alias (Alias, NoAlias, name))
import qualified Stage1.Tree.ExportSymbol as Stage1.Export
import qualified Stage1.Tree.Exports as Stage1 (Exports (..))
import qualified Stage1.Tree.Import as Stage1 (Import (..))
import Stage1.Tree.ImportFields (Fields (..))
import qualified Stage1.Tree.ImportFields as Stage1 (Fields (AllFields, Fields))
import qualified Stage1.Tree.ImportSymbol as Stage1.Import
import qualified Stage1.Tree.ImportSymbols as Stage1 (Symbols (..))
import Stage1.Tree.Marked (Marked (..))
import Stage1.Tree.Qualification (Qualification (..))
import Stage1.Variable
  ( ConstructorIdentifier,
    FullQualifiers ((:..)),
    Name (Constructor, Variable),
    QualifiedConstructorIdentifier ((:=.)),
    QualifiedVariable (..),
    Qualifiers (Local, (:.)),
    Variable,
    prelude,
    prelude',
  )
import qualified Stage1.Variable as Variable (Constructor)
import qualified Stage2.Resolve.Bindings as Regular (Bindings)
import qualified Stage2.Resolve.Bindings as Regular.Bindings
import Stage2.Resolve.Builtin (builtin)
import qualified Stage2.Resolve.Canonical as Regular (Canonical)
import qualified Stage2.Resolve.Canonical as Regular.Canonical
import qualified Stage2.Resolve.Core as Regular (Core)
import qualified Stage2.Resolve.Core as Regular.Core
import qualified Stage2.Resolve.Detail.Binding.Constructor as Detail.Constructor
import qualified Stage2.Resolve.Detail.Binding.Term as Detail.Term
import qualified Stage2.Resolve.Detail.Binding.Type as Detail.Type
import qualified Stage2.Resolve.Functor.Binding.Constructor as Functor.Constructor
import qualified Stage2.Resolve.Functor.Binding.Term as Functor.Term
import qualified Stage2.Resolve.Functor.Binding.Type as Functor.Type
import Stage2.Resolve.Functor.Bindings (Bindings (..), (!-), (!=), (!=.))
import qualified Stage2.Resolve.Functor.Bindings as Bindings
import Stage2.Resolve.Functor.Canonical (Canonical (Canonical, runCanonical), (!))
import Stage2.Resolve.Functor.Core (Core (..))
import qualified Stage2.Resolve.Functor.Core as Core
import Stage2.Resolve.Functor.Same (Same (..))
import Stage2.Resolve.Stability (Stability (..))

newtype Algebra3A f a b c = Algebra3A
  { runAlgebra3A :: forall m. (Monad m) => f (m a) (m b) (m c) -> m a
  }

newtype Algebra3B f a b c = Algebra3B
  { runAlgebra3B :: forall m. (Monad m) => f (m a) (m b) (m c) -> m b
  }

newtype Algebra3C f a b c = Algebra3C
  { runAlgebra3C :: forall m. (Monad m) => f (m a) (m b) (m c) -> m c
  }

instance (Same a) => Same (Algebra3A f a b c) where
  same abort (Algebra3A leftRun) (Algebra3A rightRun) =
    Algebra3A $ \spreadsheet -> do
      left <- leftRun spreadsheet
      right <- rightRun spreadsheet
      pure $ same abort left right

instance (Same b) => Same (Algebra3B f a b c) where
  same abort (Algebra3B leftRun) (Algebra3B rightRun) =
    Algebra3B $ \spreadsheet -> do
      left <- leftRun spreadsheet
      right <- rightRun spreadsheet
      pure $ same abort left right

instance (Same c) => Same (Algebra3C f a b c) where
  same abort (Algebra3C leftRun) (Algebra3C rightRun) =
    Algebra3C $ \spreadsheet -> do
      left <- leftRun spreadsheet
      right <- rightRun spreadsheet
      pure $ same abort left right

newtype Dependency stability request scope = Dependency
  { runDependency ::
      Bindings
        stability
        ( Void,
          Algebra3A
            request
            (Detail.Term.Binding scope)
            (Detail.Constructor.Binding scope)
            (Detail.Type.Binding scope)
        )
        ( Void,
          Algebra3B
            request
            (Detail.Term.Binding scope)
            (Detail.Constructor.Binding scope)
            (Detail.Type.Binding scope)
        )
        ( Void,
          Algebra3C
            request
            (Detail.Term.Binding scope)
            (Detail.Constructor.Binding scope)
            (Detail.Type.Binding scope)
        )
  }

instance (Semigroup stability) => Semigroup (Dependency stability request scope) where
  Dependency left <> Dependency right = Dependency $ left <> right

prefer :: Dependency stability request scope -> Dependency stability request scope -> Dependency stability request scope
prefer (Dependency left) (Dependency right) = Dependency $ Bindings.unionWith const const const const left right

updateStability :: stability -> Dependency stability' request scope -> Dependency stability request scope
updateStability stability (Dependency dependency) = Dependency (Bindings.updateStability stability dependency)

constant' :: Regular.Bindings stability scope -> Dependency stability request scope
constant' bindings = constant (Regular.Bindings.toFunctor bindings)

constant ::
  Bindings
    stability
    (Detail.Term.Binding scope)
    (Detail.Constructor.Binding scope)
    (Detail.Type.Binding scope) ->
  Dependency stability request scope
constant bindings =
  Dependency $
    trimap
      (unfailable . algebra3A)
      (unfailable . algebra3B)
      (unfailable . algebra3C)
      bindings
  where
    unfailable value = (undefined, value)
    algebra3A value = Algebra3A $ \_ -> pure value
    algebra3B value = Algebra3B $ \_ -> pure value
    algebra3C value = Algebra3C $ \_ -> pure value

strict ::
  (Trifunctor request, Monad m) =>
  request
    (m (Detail.Term.Binding scope))
    (m (Detail.Constructor.Binding scope))
    (m (Detail.Type.Binding scope)) ->
  Dependency stability request scope ->
  Bindings
    stability
    (m (Detail.Term.Binding scope))
    (m (Detail.Constructor.Binding scope))
    (m (Detail.Type.Binding scope))
strict request = trimap (runAlgebra3A . snd) (runAlgebra3B . snd) (runAlgebra3C . snd) . runDependency
  where
    runAlgebra3A (Algebra3A run) = run request
    runAlgebra3B (Algebra3B run) = run request
    runAlgebra3C (Algebra3C run) = run request

newtype Contramap request' scope request
  = Contramap
      ( forall m.
        (Monad m) =>
        request'
          (m (Detail.Term.Binding scope))
          (m (Detail.Constructor.Binding scope))
          (m (Detail.Type.Binding scope)) ->
        request
          (m (Detail.Term.Binding scope))
          (m (Detail.Constructor.Binding scope))
          (m (Detail.Type.Binding scope))
      )

contramap ::
  Contramap request' scope request ->
  Dependency stability request scope ->
  Dependency stability request' scope
contramap (Contramap map) (Dependency dependency) =
  Dependency
    ( trimap
        (fmap $ \(Algebra3A run) -> Algebra3A $ run . map)
        (fmap $ \(Algebra3B run) -> Algebra3B $ run . map)
        (fmap $ \(Algebra3C run) -> Algebra3C $ run . map)
        dependency
    )

reselectTerm ::
  Position ->
  Variable ->
  Functor.Term.Binding () ->
  Functor.Term.Binding (Void, Algebra3A (Bindings stability) a b c)
reselectTerm position name = fmap $ const (cyclicalImports position, algebra)
  where
    algebra = Algebra3A $ \canonical -> Functor.Term.value $ canonical !- position :@ name

reselectConstructor ::
  Position ->
  Variable.Constructor ->
  Functor.Constructor.Binding () ->
  Functor.Constructor.Binding (Void, Algebra3B (Bindings stability) a b c)
reselectConstructor position name = fmap $ const (cyclicalImports position, algebra)
  where
    algebra = Algebra3B $ \canonical -> Functor.Constructor.value $ canonical != position :@ name

reselectType ::
  Position ->
  ConstructorIdentifier ->
  Functor.Type.Binding () ->
  Functor.Type.Binding (Void, Algebra3C (Bindings stability) a b c)
reselectType position name = fmap $ const (cyclicalImports position, algebra)
  where
    algebra = Algebra3C $ \canonical -> Functor.Type.value $ canonical !=. position :@ name

pickTerm ::
  (Monad m) =>
  Position ->
  Variable ->
  m (Dependency () (Bindings ()) scope)
pickTerm position name =
  let term =
        Functor.Term.Binding
          { position,
            value
          }
        where
          value = (cyclicalImports position, algebra)
          algebra = Algebra3A $ \bindings -> Functor.Term.value $ bindings !- position :@ name
   in pure $
        Dependency $
          Bindings
            { terms = Map.singleton name term,
              constructors = Map.empty,
              types = Map.empty,
              stability = ()
            }

pickData ::
  (Monad m) =>
  Position ->
  ConstructorIdentifier ->
  Fields ->
  m (Bindings stability () () ()) ->
  m (Dependency () (Bindings ()) scope)
pickData position name AllFields request = do
  Bindings {terms, constructors, types} <- request
  let term name =
        Functor.Term.Binding
          { position = Functor.Term.position (terms Map.! name),
            value
          }
        where
          value = (cyclicalImports position, algebra)
          algebra = Algebra3A $ \bindings -> Functor.Term.value $ bindings !- position :@ name
      constructor name =
        Functor.Constructor.Binding
          { position = Functor.Constructor.position (constructors Map.! name),
            value
          }
        where
          value = (cyclicalImports position, algebra)
          algebra = Algebra3B $ \bindings -> Functor.Constructor.value $ bindings != position :@ name
      typex =
        Functor.Type.Binding
          { position,
            value,
            fields = Functor.Type.fields $ types Map.! name,
            constructors = Functor.Type.constructors $ types Map.! name
          }
        where
          value = (cyclicalImports position, algebra)
          algebra = Algebra3C $ \bindings -> Functor.Type.value $ bindings !=. position :@ name
  case Map.lookup name types of
    Just Functor.Type.Binding {fields, constructors} -> do
      pure $
        Dependency $
          Bindings
            { terms = Map.fromSet term fields,
              constructors = Map.fromSet constructor constructors,
              types = Map.singleton name typex,
              stability = ()
            }
    Nothing -> typeNotInScope position
pickData position name Fields {picks} _ = pure $ Dependency acyclic
  where
    forceType :: (Monad m) => Bindings () a b (m c) -> m ()
    forceType bindings = do
      _ <- Functor.Type.value $ bindings !=. position :@ name
      pure ()
    constructors =
      Map.fromList [(name, position) | position :@ Constructor name <- toList picks]
    fields =
      Map.fromList [(name, position) | position :@ Variable name <- toList picks]

    term name position =
      Functor.Term.Binding
        { position,
          value
        }
      where
        value = (cyclicalImports position, algebra)
        algebra = Algebra3A $ \bindings ->
          forceType bindings >> do
            Functor.Term.value $ bindings !- position :@ name
    constructor name position =
      Functor.Constructor.Binding
        { position,
          value
        }
      where
        value = (cyclicalImports position, algebra)
        algebra = Algebra3B $ \bindings ->
          forceType bindings >> do
            Functor.Constructor.value $ bindings != position :@ name
    types = Map.singleton name typex
      where
        typex =
          Functor.Type.Binding
            { position,
              value,
              constructors = Map.keysSet constructors,
              fields = Map.keysSet fields
            }
          where
            value = (cyclicalImports position, algebra)
            algebra = Algebra3C $ \bindings ->
              let checkSelector name position ()
                    | Set.member name realSelectors = ()
                    | otherwise = fieldNotInScope position name
                  checkConstructor name position ()
                    | Set.member name realConstructors = ()
                    | otherwise = constructorNotInScope position name
                  Functor.Type.Binding
                    { value,
                      fields = realSelectors,
                      constructors = realConstructors
                    } = bindings !=. position :@ name
               in if
                    | () <- Map.foldrWithKey checkSelector () fields,
                      () <- Map.foldrWithKey checkConstructor () constructors ->
                        value
    acyclic =
      Bindings
        { terms = Map.mapWithKey term fields,
          constructors = Map.mapWithKey constructor constructors,
          types,
          stability = ()
        }

pickSymbol ::
  (Monad m) =>
  Stage1.Import.Symbol ->
  m (Bindings stability () () ()) ->
  m (Dependency () (Bindings ()) scope)
pickSymbol Stage1.Import.Definition {variable = startPosition :@ variable} _ =
  pickTerm startPosition variable
pickSymbol
  Stage1.Import.Data
    { typeVariable = startPosition :@ typeVariable,
      fields
    }
  request =
    pickData startPosition typeVariable fields request

-- stolen from profunctors
dimap :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
dimap f g go = g . go . f

dimapIdentity :: (Functor f1, Functor f2) => (f2 (Identity a) -> f1 (Identity b)) -> f2 a -> f1 b
dimapIdentity = dimap (fmap Identity) (fmap runIdentity)

dimapIdentity3 ::
  (Trifunctor f1, Trifunctor f2) =>
  ( f1 (Identity a1) (Identity a2) (Identity a3) ->
    f2 (Identity b1) (Identity b2) (Identity b3)
  ) ->
  f1 a1 a2 a3 ->
  f2 b1 b2 b3
dimapIdentity3 pick = run . pick . wrap
  where
    run = trimap runIdentity runIdentity runIdentity
    wrap = trimap Identity Identity Identity

dimapImport ::
  ( Canonical
      (Detail.Term.Binding scope1)
      (Detail.Constructor.Binding scope1)
      (Detail.Type.Binding scope1) ->
    Core
      Stability
      (Detail.Term.Binding scope2)
      (Detail.Constructor.Binding scope2)
      (Detail.Type.Binding scope2)
  ) ->
  Regular.Canonical.Canonical scope1 ->
  Regular.Core scope2
dimapImport = dimap Regular.Canonical.toFunctor Regular.Core.fromFunctor

pickStrict ::
  (Monad m) =>
  ( Map FullQualifiers (Bindings () () () ()) ->
    Map Qualifiers (Dependency Stability Canonical scope)
  ) ->
  Canonical
    (m (Detail.Term.Binding scope))
    (m (Detail.Constructor.Binding scope))
    (m (Detail.Type.Binding scope)) ->
  Core
    Stability
    (m (Detail.Term.Binding scope))
    (m (Detail.Constructor.Binding scope))
    (m (Detail.Type.Binding scope))
pickStrict pick canonical =
  Core.fromMap $ refine <$> pick lookahead
  where
    refine = strict canonical
    constant = trimap (const ()) (const ()) (const ())
    lookahead = constant <$> runCanonical canonical

normalizeImport ::
  ( Map FullQualifiers (Identity (Bindings () () () ())) ->
    Map Qualifiers (Identity (Dependency Stability Canonical scope2))
  ) ->
  Regular.Canonical.Canonical scope2 ->
  Regular.Core scope2
normalizeImport = dimapImport . dimapIdentity3 . pickStrict . dimapIdentity

pickPrelude ::
  Position ->
  [Stage1.Import Position] ->
  Regular.Canonical.Canonical scope ->
  Regular.Core scope
pickPrelude position declarations = normalizeImport $ pickPrelude' position declarations

pickPrelude' ::
  (Monad m) =>
  Position ->
  [Stage1.Import Position] ->
  Map FullQualifiers (m (Bindings () () () ())) ->
  Map Qualifiers (m (Dependency Stability Canonical scope))
pickPrelude' position declarations request = case not $ any isPrelude declarations of
  True
    | Just request <- Map.lookup prelude request ->
        let binding = do
              Bindings {terms, constructors, types} <- request
              let bindings =
                    Bindings
                      { terms = Map.mapWithKey (reselectTerm position) terms,
                        constructors = Map.mapWithKey (reselectConstructor position) constructors,
                        types = Map.mapWithKey (reselectType position) types,
                        stability = Stable [position]
                      }
              pure $ contramap (Contramap (! position :@ prelude)) $ Dependency $ bindings
         in Map.fromList [(Local, binding), (prelude', binding)]
    | otherwise -> error "no prelude"
  False -> Map.empty
  where
    isPrelude :: Stage1.Import Position -> Bool
    isPrelude (Stage1.Import {target})
      | target == prelude = True
    isPrelude _ = False

pickImports ::
  StableImports ->
  [Stage1.Import Position] ->
  Regular.Canonical.Canonical scope ->
  Regular.Core.Core scope
pickImports stableImports declarations = normalizeImport (pickImports' stableImports declarations)

newtype StableImports = StableImports Bool

pickImports' ::
  (Monad m) =>
  StableImports ->
  [Stage1.Import Position] ->
  Map FullQualifiers (m (Bindings () () () ())) ->
  Map Qualifiers (m (Dependency Stability Canonical scope))
pickImports' (StableImports stableImports) declarations request = Map.fromListWith (liftM2 (<>)) $ do
  Stage1.Import
    { qualification,
      targetPosition = position,
      target = name,
      alias,
      symbols
    } <-
    declarations
  let qualifiedName = case alias of
        Stage1.NoAlias | root :.. name <- name -> root :. name
        Stage1.Alias {name} | root :.. name <- name -> root :. name
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
            { terms = Map.mapWithKey (reselectTerm position) terms,
              constructors = Map.mapWithKey (reselectConstructor position) constructors,
              types = Map.mapWithKey (reselectType position) types,
              stability
            }
      bindings = case symbols of
        Stage1.Symbols {symbols} -> update <$> foldr combine empty items
          where
            combine = liftM2 (<>)
            empty = pure $ Dependency $ mempty
            items = [contramap (Contramap (! position :@ name)) <$> pickSymbol symbol base | symbol <- toList symbols]
            update = updateStability (Stable [position])
        Stage1.All -> contramap (Contramap (! position :@ name)) . Dependency <$> all
        Stage1.Hiding {symbols} -> do
          base@Bindings {terms, constructors, types, stability} <- all
          let bindings =
                Bindings
                  { terms = foldr Map.delete terms termDeletions,
                    constructors = foldr Map.delete constructors constructorDeletions,
                    types = foldr Map.delete types typeDeletions,
                    stability
                  }
              termDeletions = do
                symbol <- toList symbols
                case symbol of
                  Stage1.Import.Definition {variable = _ :@ variable} -> [variable]
                  Stage1.Import.Data {typeVariable, fields} ->
                    case fields of
                      Stage1.AllFields -> case base !=. typeVariable of
                        Functor.Type.Binding {fields} -> toList fields
                      Stage1.Fields {picks} -> do
                        _ :@ Variable name <- toList picks
                        pure name
              constructorDeletions = do
                Stage1.Import.Data {typeVariable, fields} <-
                  toList symbols
                case fields of
                  Stage1.AllFields -> case base !=. typeVariable of
                    Functor.Type.Binding {constructors} -> toList constructors
                  Stage1.Fields {picks} -> do
                    _ :@ Constructor name <- toList picks
                    pure name
              typeDeletions = do
                Stage1.Import.Data {typeVariable = _ :@ typeVariable} <- toList symbols
                pure typeVariable
          pure $ contramap (Contramap (! position :@ name)) $ Dependency $ bindings

  (qualifiedName, bindings) : case qualification of
    Qualified -> []
    Unqualified -> [(Local, bindings)]

pickExports ::
  (Monad m) =>
  Regular.Bindings () scope ->
  Stage1.Exports ->
  Map Qualifiers (m (Bindings Stability () () ())) ->
  m (Dependency () (Core ()) scope)
pickExports _ Stage1.Exports {exports} request = do
  let pick export = case export of
        Stage1.Export.Module {modulex = position :@ root :.. name} ->
          case Map.lookup (root :. name) request of
            Just request -> do
              Bindings {terms, constructors, types, stability} <- request
              let bindings =
                    Bindings
                      { terms = Map.mapWithKey (reselectTerm position) terms,
                        constructors = Map.mapWithKey (reselectConstructor position) constructors,
                        types = Map.mapWithKey (reselectType position) types,
                        stability
                      }
              pure $ contramap (Contramap (Core.! position :@ root :. name)) $ Dependency $ bindings
            Nothing -> moduleNotInScope position
        Stage1.Export.Definition {variable = position :@ root :- name} -> do
          bindings <- pickTerm position name
          pure $ contramap (Contramap (Core.! position :@ root)) $ updateStability (Stable [position]) bindings
        Stage1.Export.Data {typeVariable = position :@ root :=. name, fields} -> do
          let base = case Map.lookup root request of
                Just base -> base
                Nothing -> moduleNotInScope position
          bindings <- pickData position name fields base
          pure $ contramap (Contramap (Core.! position :@ root)) $ updateStability (Stable [position]) bindings
  bindings <- traverse pick exports
  pure $ Dependency $ Bindings.updateStability () $ foldMap runDependency bindings
pickExports _ Stage1.Builtin _ = pure $ constant' builtin
pickExports defaultx Stage1.Default _ = pure $ constant' defaultx

data Module scope = Module
  { modulePosition :: Position,
    extensions :: Stage1.Extensions,
    imports :: [Stage1.Import Position],
    exports :: Stage1.Exports,
    base :: Regular.Bindings () scope
  }

pickModules :: forall scope. Map FullQualifiers (Module scope) -> Regular.Canonical scope
pickModules modules =
  Regular.Canonical.fromFunctor $
    loeb3 $
      Loeb3 $
        Canonical $
          loeb $
            Loeb $
              let ignore :: Bindings s a b c -> Bindings s () () ()
                  ignore = trimap (const ()) (const ()) (const ())
                  go (root :.. name) Module {modulePosition, extensions, exports, imports, base} =
                    (cyclicalImports modulePosition, algebra)
                    where
                      Extensions {implicitPrelude, stableImports} = extensions
                      algebra modules = do
                        let regular = base
                            update = Bindings.updateStability (Stable [modulePosition])
                        base <- pure $ update $ Regular.Bindings.toFunctor base
                        let pick ::
                              (Monad m) =>
                              Map FullQualifiers (m (Bindings () () () ())) ->
                              Map Qualifiers (m (Dependency Stability Canonical scope))
                            pick request =
                              let base = pickImports' (StableImports stableImports) imports request
                                  prelude =
                                    if implicitPrelude
                                      then pickPrelude' modulePosition imports request
                                      else Map.empty
                                  combined = Map.unionWith (liftM2 (<>)) base prelude
                                  shadower = pure <$> shadow
                               in Map.unionWith (liftM2 prefer) shadower combined
                            shadow = Map.fromList [(Local, constant base), (root :. name, constant base)]
                        let imports = pick (fmap ignore <$> modules)
                        exports <- pickExports regular exports (fmap (ignore . runDependency) <$> imports)
                        let go = Contramap (Core.updateStability () . pickStrict (dimapIdentity pick))
                            bindings = runDependency $ contramap go exports
                        pure $ trimap (fmap runAlgebra3A) (fmap runAlgebra3B) (fmap runAlgebra3C) bindings
               in Map.mapWithKey go modules
