module Error
  ( HazyError,
    Type,
    types,
    fail,
    allow,
    digit,
    octal,
    hexadecimal,
    integer,
    decimal,
    string,
    pathCharacter,
    endOfFile,
    nonSymbol,
    newline,
    character,
    stringCharacter,
    whitespace,
    whitespaceNoLine,
    variableIdentifier,
    qualifiedVariableIdentifier,
    constructorIdentifier,
    qualifiedConstructorIdentifier,
    variableSymbol,
    qualifiedVariableSymbol,
    constructorSymbol,
    qualifiedConstructorSymbol,
    moduleIdentifier,
    braceIdentionError,
    parenIdentionError,
    bracketIdentionError,
    pragmaIdentionError,
    Position,
    locate,
    locateHint,
    expected,
    missingMethodEntry,
    missingVariableEntry,
    missingConstructorDeclaration,
    missingTypeDeclaration,
    duplicateVariableEntries,
    duplicateConstructorEntries,
    duplicateTypeEntries,
    duplicateMethodEntries,
    duplicateFixityEntries,
    duplicateAnnotationEntries,
    duplicateFieldEntries,
    duplicateModuleEntries,
    duplicateUnorderedEntries,
    mismatchedModuleNames,
    moduleNotFound,
    moduleNotInScope,
    variableNotInScope,
    typeNotInScope,
    constructorNotInScope,
    fieldNotInScope,
    cannotTurnoffExtension,
    mismatchSelectors,
    unstableImports,
    unstableShadowing,
    orderDependentUsage,
    cyclicalImports,
    unificationError,
    nonUniqueConstraints,
    constraintError,
    ambiguousType,
    escapingType,
    illegalInstanceClass,
    illegalInstanceData,
    illegalFixity,
    constraintNonLocal,
    illegalConstraint,
    mismatchSelectorTypes,
    badRunSTCall,
    overlappingInstances,
    patternInMethod,
    patternInInstance,
    notClassMethod,
    instanceForNonClass,
    unneededFieldQualification,
    occurenceError,
    cyclicalTypeChecking,
    mismatchedConstructorArguments,
    universeMustBeSmall,
    uncheckable,
    unsupportedFeatureExpressionAnnotation,
    unsupportedFeatureRunST,
    unsupportedFeatureRightSection,
    unsupportedFeatureDoNotation,
    unsupportedFeatureLambdaCase,
    unsupportedFeatureRecordUpdate,
    unsupportedFeatureListComprehension,
    unsupportedFeatureConstraintedTypeDefaulting,
    unsupportedFeaturePatternLetBinds,
    unsupportedFeatureStrictFunctions,
    unsupportedFeatureIntegerLiteralPatterns,
    unsupportedFeatureFloatingPointLiterals,
    unsupportedFeaturePolymorphicComponents,
    unsupportedFeatureGADTs,
  )
where

import Control.Exception (Exception, throw)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Stage1.ParserCombinator
  ( Position,
    column,
    file,
    line,
  )
import System.Exit (exitFailure)
import Prelude hiding (fail)

data HazyError = HazyError !Type Lazy.Text
  deriving (Show)

instance Exception HazyError

hazyError :: Type -> Lazy.Text -> a
hazyError = (throw .) . HazyError

fail :: HazyError -> IO ()
fail (HazyError _ message) = do
  Text.Lazy.IO.putStrLn message
  exitFailure

allow :: Type -> HazyError -> IO ()
allow typex' error@(HazyError typex _)
  | typex == typex' = pure ()
  | otherwise = throw error

digit = fromString "digit"

octal = fromString "octal"

hexadecimal = fromString "hexadecimal"

integer = fromString "integer"

decimal = fromString "decimal"

string = fromString "string"

pathCharacter = fromString "path character"

endOfFile = fromString "end of file"

nonSymbol = fromString "non symbol"

newline = fromString "newline"

character = fromString "character"

stringCharacter = fromString "string character"

whitespace = fromString "whitespace"

whitespaceNoLine = fromString "non-line whitespace"

annotation = fromString "annotation"

variable = fromString "variable"

fixity = fromString "fixity"

variableIdentifier = fromString "variable identifier"

qualifiedVariableIdentifier = fromString "qualified variable identifier"

constructor = fromString "constructor"

constructorIdentifier = fromString "constructor identifier"

qualifiedConstructorIdentifier = fromString "qualified constructor identifier"

variableSymbol = fromString "variable symbol"

qualifiedVariableSymbol = fromString "qualified variable symbol"

constructorSymbol = fromString "constructor symbol"

qualifiedConstructorSymbol = fromString "qualified constructor symbol"

moduleIdentifier = fromString "module identifier"

field = fromString "field"

method = fromString "method"

typex = fromString "type"

braceIdentionError = fromString "brace indentation error"

parenIdentionError = fromString "parenthesis indentation error"

bracketIdentionError = fromString "bracket indentation error"

pragmaIdentionError = fromString "bracket indentation error"

modulex = fromString "module"

languagePragma = fromString "language pragma"

unorderedPragma = fromString "unordered pragma"

locate :: Position -> Builder
locate position =
  mconcat
    [ fromText (file position),
      fromString ":",
      fromString (show (line position)),
      fromString ":",
      fromString (show (column position))
    ]

formatError :: Position -> Builder -> Builder
formatError position error = mconcat builders
  where
    builders =
      [ locate position,
        fromString " error: ",
        error
      ]

locateHint :: Position -> Builder -> Builder
locateHint position error =
  mconcat
    [ fromString "(at ",
      locate position,
      fromString ") ",
      error
    ]

data Type
  = Expected
  | MissingVariableDeclaration
  | MissingMethodDeclaration
  | MissingConstructorDeclaration
  | MissingTypeDeclaration
  | DuplicateVariableEntries
  | DuplicateFixityEntries
  | DuplicateAnnotationEntries
  | DuplicateConstructorEntries
  | DuplicateMethodEntries
  | DuplicateFieldEntries
  | DuplicateTypeEntries
  | DuplicateModuleEntries
  | DuplicateUnordereds
  | MismatchModuleNames
  | ModuleNotFound
  | ModuleNotInScope
  | VariableNotInScope
  | TypeNotInScope
  | ConstructorNotInScope
  | FieldNotInScope
  | CannotTurnOffExtension
  | MismatchSelectors
  | UnstableImport
  | UnstableShadowing
  | OrderDependentUsage
  | CyclicalImports
  | UnificationError
  | NonUniqueConstraints
  | ConstraintError
  | AmbiguousType
  | EscapingType
  | IllegalInstanceClass
  | IllegalInstanceData
  | IllegalFixity
  | RecordSyntaxOnBuiltin
  | MismatchedSelectorTypes
  | ConstraintNonLocal
  | IllegalConstraint
  | BadRunSTCall
  | OverlappingInstances
  | PatternInMethod
  | PatternInInstance
  | NotClassMethod
  | InstanceForNonClass
  | UnneededFieldQualification
  | OccurenceError
  | CyclicalTypeChecking
  | MismatchedConstructorArguments
  | UniverseMustBeSmall
  | Uncheckable
  | UnsupportedFeature
  deriving (Show, Eq, Enum, Bounded)

types :: Map String Type
types = Map.fromList [(show error, error) | error <- [minBound .. maxBound]]

errorAt :: Type -> Position -> Builder -> a
errorAt typex position message = hazyError typex $ toLazyText $ formatError position message

expected :: Position -> [Builder] -> Builder -> a
expected position expect found = errorAt Expected position (mconcat builders)
  where
    builders =
      [ fromString "expected ",
        many,
        mconcat tokens,
        fromString " but found ",
        found
      ]
    tokens = intersperse (fromString ", ") expect
    many = case expect of
      [_] -> mempty
      _ -> fromString "one of "

missingDeclaration :: Type -> Builder -> Position -> a
missingDeclaration typex brand position =
  errorAt typex position $
    mconcat
      [ fromString "missing ",
        brand,
        fromString " declaration"
      ]

missingVariableEntry = missingDeclaration MissingVariableDeclaration variable

missingMethodEntry = missingDeclaration MissingMethodDeclaration method

missingConstructorDeclaration = missingDeclaration MissingConstructorDeclaration constructor

missingTypeDeclaration = missingDeclaration MissingTypeDeclaration typex

duplicateEntries :: Type -> Builder -> [Position] -> a
duplicateEntries typex brand entries =
  errorAt typex (head entries) (mconcat builders)
  where
    builders =
      [ fromString "duplicate ",
        brand,
        fromString " declarations at ",
        mconcat $ intersperse (fromString ", ") (locate <$> tail entries)
      ]

duplicateVariableEntries = duplicateEntries DuplicateVariableEntries variable

duplicateFixityEntries = duplicateEntries DuplicateFixityEntries fixity

duplicateAnnotationEntries = duplicateEntries DuplicateAnnotationEntries annotation

duplicateConstructorEntries = duplicateEntries DuplicateConstructorEntries constructor

duplicateMethodEntries = duplicateEntries DuplicateMethodEntries method

duplicateFieldEntries = duplicateEntries DuplicateFieldEntries field

duplicateTypeEntries = duplicateEntries DuplicateTypeEntries typex

duplicateModuleEntries = duplicateEntries DuplicateModuleEntries modulex

duplicateUnorderedEntries = duplicateEntries DuplicateUnordereds unorderedPragma

mismatchedModuleNames :: Position -> a
mismatchedModuleNames position = errorAt MismatchModuleNames position message
  where
    message = fromString "module name doesn't match file name"

moduleNotFound :: Position -> a
moduleNotFound position = errorAt ModuleNotFound position message
  where
    message = fromString "module not found"

notInScope :: Type -> Builder -> Position -> a
notInScope typex brand position = errorAt typex position (mconcat builders)
  where
    builders =
      [ brand,
        fromString " not in scope"
      ]

moduleNotInScope = notInScope ModuleNotInScope modulex

variableNotInScope = notInScope VariableNotInScope variable

typeNotInScope = notInScope TypeNotInScope typex

constructorNotInScope = notInScope ConstructorNotInScope constructor

fieldNotInScope = notInScope FieldNotInScope field

cannotTurnoffExtension :: Position -> Builder -> a
cannotTurnoffExtension position extension =
  errorAt CannotTurnOffExtension position (mconcat builders)
  where
    builders =
      [ fromString "cannot turn off ",
        extension
      ]

mismatchSelectors :: Position -> a
mismatchSelectors position = errorAt MismatchSelectors position $ fromString "mismatched selectors"

unstableImports :: [Position] -> a
unstableImports positions = errorAt UnstableImport (head positions) $ mconcat builders
  where
    builders =
      [ fromString "at most one import may be unstable: ",
        mconcat $ intersperse (fromString ", ") $ map locate (tail positions)
      ]

unstableShadowing :: Position -> a
unstableShadowing position =
  errorAt UnstableShadowing position $ fromString "local unstable imports are not supported"

orderDependentUsage :: Position -> a
orderDependentUsage position =
  errorAt OrderDependentUsage position $ fromString "order dependent usage of record constructor"

cyclicalImports :: Position -> a
cyclicalImports position =
  errorAt CyclicalImports position $ fromString "unresolvable cyclical imports"

unificationError :: Position -> Builder -> Builder -> a
unificationError position left right =
  errorAt UnificationError position $ mconcat builders
  where
    builders =
      [ fromString "unification error: couldn't match `",
        left,
        fromString "` with `",
        right,
        fromString "`"
      ]

nonUniqueConstraints :: Position -> a
nonUniqueConstraints position =
  errorAt NonUniqueConstraints position $
    fromString
      "constraints include non-unique typeclass variable pair"

constraintError :: Position -> Builder -> a
constraintError position satify =
  errorAt ConstraintError position $ mconcat builders
  where
    builders =
      [ fromString "constraint error: unable to satify `",
        satify,
        fromString "`"
      ]

ambiguousType :: Position -> a
ambiguousType position =
  errorAt AmbiguousType position $ fromString "ambiguous type"

escapingType :: Position -> Builder -> a
escapingType position typex =
  errorAt EscapingType position $ mconcat builders
  where
    builders =
      [ fromString "escaping type: `",
        typex,
        fromString "`"
      ]

illegalInstanceClass :: Position -> a
illegalInstanceClass position =
  errorAt IllegalInstanceClass position $ fromString "illegal instance class"

illegalInstanceData :: Position -> a
illegalInstanceData position =
  errorAt IllegalInstanceData position $ fromString "illegal instance data"

illegalFixity :: Position -> a
illegalFixity position =
  errorAt IllegalFixity position $ fromString "illegal fixity"

constraintNonLocal position =
  errorAt ConstraintNonLocal position $ fromString "constraint with non local variable"

illegalConstraint :: Position -> a
illegalConstraint position =
  errorAt IllegalConstraint position $ fromString "illegal constraint"

mismatchSelectorTypes :: Position -> a
mismatchSelectorTypes position =
  errorAt MismatchedSelectorTypes position $ fromString "mismatched selector types"

badRunSTCall :: Position -> b
badRunSTCall position =
  errorAt BadRunSTCall position $ fromString "bad runST call"

overlappingInstances :: [Position] -> a
overlappingInstances positions = errorAt OverlappingInstances (head positions) (mconcat builders)
  where
    builders =
      [ fromString "overlapping instance definitions:",
        mconcat $ intersperse (fromString ", ") (locate <$> tail positions)
      ]

patternInMethod :: Position -> a
patternInMethod position = errorAt PatternInMethod position $ fromString "pattern in method definition"

patternInInstance :: Position -> a
patternInInstance position = errorAt PatternInInstance position $ fromString "pattern in instance definition"

notClassMethod :: Position -> a
notClassMethod position = errorAt NotClassMethod position $ fromString "instance definition not a class method"

instanceForNonClass :: Position -> a
instanceForNonClass position = errorAt InstanceForNonClass position $ fromString "instance declaration for non-class"

unneededFieldQualification :: Position -> a
unneededFieldQualification position =
  errorAt UnneededFieldQualification position $ fromString "unneed field qualification"

occurenceError :: Position -> Builder -> Builder -> a
occurenceError position infinite term =
  errorAt OccurenceError position $ mconcat builders
  where
    builders =
      [ fromString "occurance error: `",
        infinite,
        fromString "` appears in `",
        term,
        fromString "`"
      ]

cyclicalTypeChecking :: Position -> a
cyclicalTypeChecking position =
  errorAt CyclicalTypeChecking position $ fromString "cyclical type checking"

universeMustBeSmall :: Position -> a
universeMustBeSmall position = errorAt UniverseMustBeSmall position $ fromString "universe must be small"

uncheckable :: Position -> a
uncheckable position = errorAt Uncheckable position $ fromString "not typecheckable"

mismatchedConstructorArguments :: Position -> a
mismatchedConstructorArguments position =
  errorAt MismatchedConstructorArguments position $ fromString "mismatched number of constructor arguments"

listComprehension = fromString "list comprehension"

integerLiteralPatterns = fromString "integer literal patterns"

floatingPointLiterals = fromString "floating point literals"

strictFunctions = fromString "strict functions"

gadts = fromString "GADTs"

polymorphicComponents = fromString "PolymorphicComponents"

patternLetBinds = fromString "patternLetBinds"

constraintedTypeDefaulting = fromString "constrained type defaulting"

recordUpdate = fromString "record update"

doNotation = fromString "do notation"

lambdaCase = fromString "lambda case"

rightSection = fromString "right section"

runST = fromString "runST"

expressionAnnotations = fromString "expression annotations"

unsupportedFeature :: Builder -> Position -> a
unsupportedFeature feature position =
  errorAt UnsupportedFeature position $ fromString "unsupported feature: " <> feature

unsupportedFeatureRunST :: Position -> a
unsupportedFeatureRunST = unsupportedFeature runST

unsupportedFeatureExpressionAnnotation :: Position -> a
unsupportedFeatureExpressionAnnotation =
  unsupportedFeature expressionAnnotations

unsupportedFeatureRightSection :: Position -> a
unsupportedFeatureRightSection =
  unsupportedFeature rightSection

unsupportedFeatureDoNotation :: Position -> a
unsupportedFeatureDoNotation =
  unsupportedFeature doNotation

unsupportedFeatureLambdaCase :: Position -> a
unsupportedFeatureLambdaCase =
  unsupportedFeature lambdaCase

unsupportedFeatureRecordUpdate :: Position -> a
unsupportedFeatureRecordUpdate =
  unsupportedFeature recordUpdate

unsupportedFeatureListComprehension :: Position -> a
unsupportedFeatureListComprehension =
  unsupportedFeature listComprehension

unsupportedFeatureConstraintedTypeDefaulting :: Position -> a
unsupportedFeatureConstraintedTypeDefaulting =
  unsupportedFeature constraintedTypeDefaulting

unsupportedFeaturePatternLetBinds :: Position -> a
unsupportedFeaturePatternLetBinds =
  unsupportedFeature patternLetBinds

unsupportedFeatureStrictFunctions :: Position -> a
unsupportedFeatureStrictFunctions =
  unsupportedFeature strictFunctions

unsupportedFeatureIntegerLiteralPatterns :: Position -> a
unsupportedFeatureIntegerLiteralPatterns =
  unsupportedFeature integerLiteralPatterns

unsupportedFeatureFloatingPointLiterals :: Position -> a
unsupportedFeatureFloatingPointLiterals =
  unsupportedFeature floatingPointLiterals

unsupportedFeaturePolymorphicComponents :: Position -> a
unsupportedFeaturePolymorphicComponents =
  unsupportedFeature polymorphicComponents

unsupportedFeatureGADTs :: Position -> a
unsupportedFeatureGADTs = unsupportedFeature gadts
