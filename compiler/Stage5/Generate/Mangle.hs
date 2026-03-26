{-# LANGUAGE_HAZY UnorderedRecords #-}
module Stage5.Generate.Mangle where

import Control.Applicative (liftA)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import qualified Data.Text.Lazy.Builder as Builder
import Javascript.Keywords (keywords)
import Stage1.Lexer
  ( FullQualifiers (..),
    Qualifiers (..),
    runConstructorIdentifier,
    runVariableIdentifier,
    runVariableSymbol,
  )
import Stage1.Variable (ConstructorIdentifier, FullyQualifiedConstructorIdentifier (..), Variable (..))
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage4.Tree.TermDeclaration (Name (..))
import System.FilePath ((</>))

letters, letters' :: [Char]
letters = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['$', '_']
letters' = letters ++ ['0' .. '9']

names :: [Text]
names = filter (`Set.notMember` keywords) $ map pack $ go [""]
  where
    go :: [String] -> [String]
    go post =
      let set = (:) <$> letters <*> post
          next = (:) <$> letters' <*> post
       in set ++ go next

fields :: [Text]
fields = names

data Brand
  = Class
  | Data

mangle :: Name -> Text
mangle = \case
  Name (VariableIdentifier name) -> runVariableIdentifier name
  Name (VariableSymbol name) -> runVariableSymbol name
  Unnamed index -> pack $ show index

mangleInstance ::
  (Type.Index scope -> FullyQualifiedConstructorIdentifier) ->
  Brand ->
  ConstructorIdentifier ->
  Type2.Index scope ->
  Text
mangleInstance run brand name target = Text.Lazy.toStrict $ Builder.toLazyText builder
  where
    builder =
      mconcat
        [ fromString "instance ",
          classx,
          fromString " ",
          datax
        ]
    classx = case brand of
      Class -> owner
      Data -> for target
    datax = case brand of
      Data -> owner
      Class -> for target
    owner = fromText $ runConstructorIdentifier name
    for target = case target of
      Type2.Index index -> qualify (run index)
      Type2.Lifted Constructor.Index {typeIndex, constructorIndex} ->
        -- todo mention constructor name in instance mangling
        for typeIndex <> fromString "." <> fromString (show constructorIndex)
      Type2.Bool -> fromString "Hazy.Bool"
      Type2.Char -> fromString "Hazy.Char"
      Type2.ST -> fromString "Hazy.ST"
      Type2.Arrow -> fromString "(->)"
      Type2.List -> fromString "[]"
      Type2.Tuple n -> mconcat [fromString "(", mconcat $ replicate n $ fromString ",", fromString ")"]
      Type2.Integer -> fromString "Hazy.Integer"
      Type2.Int -> fromString "Hazy.Int"
      Type2.Ordering -> fromString "Hazy.Ordering"
      Type2.Ratio -> fromString "Hazy.Ratio"
      Type2.Num -> fromString "Hazy.Num"
      Type2.Enum -> fromString "Hazy.Enum"
      Type2.Eq -> fromString "Hazy.Eq"
      Type2.Ord -> fromString "Hazy.Ord"
      Type2.Real -> fromString "Hazy.Real"
      Type2.Integral -> fromString "Hazy.Integral"
      Type2.Fractional -> fromString "Hazy.Fractional"
      Type2.Functor -> fromString "Hazy.Functor"
      Type2.Applicative -> fromString "Hazy.Applicative"
      Type2.Monad -> fromString "Hazy.Monad"
      Type2.MonadFail -> fromString "Hazy.MonadFail"
    qualify :: FullyQualifiedConstructorIdentifier -> Builder
    qualify (Local :.. root :.=. name) =
      mconcat
        [ fromText (runConstructorIdentifier root),
          fromString ".",
          fromText (runConstructorIdentifier name)
        ]
    qualify (path :. next :.. root :.=. name) =
      mconcat
        [ fromText (runConstructorIdentifier next),
          fromString ".",
          qualify (path :.. root :.=. name)
        ]

lazy = pack "a"

value = pack "b"

local = pack "a"

pathSys :: FullQualifiers -> FilePath
pathSys = foldr1 (</>) . fmap unpack . path'

pathJS :: FullQualifiers -> Text
pathJS = foldr1 (\x y -> x <> pack "/" <> y) . path'

path' :: FullQualifiers -> NonEmpty Text
path' (root :.. head) = go (runConstructorIdentifier head :| []) root
  where
    go result Local = result
    go result (root :. head) =
      go (NonEmpty.cons (runConstructorIdentifier head) result) root

depth :: FullQualifiers -> Text
depth (Local :.. _) = pack "./"
depth (path :. _ :.. root) = depth (path :.. root) <> pack "../"

mjs = pack ".mjs"

runtime = pack "Hazy.mjs"

data Builtin a = Builtin
  { abort,
    numInt,
    numInteger,
    enumBool,
    enumChar,
    enumInt,
    enumInteger,
    eqBool,
    eqChar,
    eqTuple,
    eqInt,
    eqInteger,
    eqOrdering,
    ordInt,
    ordInteger,
    functorList,
    applicativeList,
    monadList,
    monadFailList,
    defaultPlus,
    defaultMinus,
    defaultMultiply,
    defaultNegate,
    defaultAbs,
    defaultSignum,
    defaultFromInteger,
    defaultSucc,
    defaultPred,
    defaultToEnum,
    defaultFromEnum,
    defaultEnumFrom,
    defaultEnumFromThen,
    defaultEnumFromTo,
    defaultEnumFromThenTo,
    defaultEqual,
    defaultNotEqual,
    defaultCompare,
    defaultLessThen,
    defaultLessThenEqual,
    defaultGreaterThen,
    defaultGreaterThenEqual,
    defaultMax,
    defaultMin,
    defaultToRational,
    defaultQuot,
    defaultRem,
    defaultDiv,
    defaultMod,
    defaultQuotRem,
    defaultDivMod,
    defaultToInteger,
    defaultDivide,
    defaultRecip,
    defaultFromRational,
    defaultFmap,
    defaultFconst,
    defaultPure,
    defaultAp,
    defaultLiftA2,
    defaultDiscardLeft,
    defaultDiscardRight,
    defaultBind,
    defaultThen,
    defaultReturn,
    defaultFail ::
      a
  }

instance Functor Builtin where
  fmap = liftA

instance Applicative Builtin where
  pure a =
    Builtin
      { abort = a,
        numInt = a,
        numInteger = a,
        enumBool = a,
        enumChar = a,
        enumInt = a,
        enumInteger = a,
        eqBool = a,
        eqChar = a,
        eqTuple = a,
        eqInt = a,
        eqInteger = a,
        eqOrdering = a,
        ordInt = a,
        ordInteger = a,
        functorList = a,
        applicativeList = a,
        monadList = a,
        monadFailList = a,
        defaultPlus = a,
        defaultMinus = a,
        defaultMultiply = a,
        defaultNegate = a,
        defaultAbs = a,
        defaultSignum = a,
        defaultFromInteger = a,
        defaultSucc = a,
        defaultPred = a,
        defaultToEnum = a,
        defaultFromEnum = a,
        defaultEnumFrom = a,
        defaultEnumFromThen = a,
        defaultEnumFromTo = a,
        defaultEnumFromThenTo = a,
        defaultEqual = a,
        defaultNotEqual = a,
        defaultCompare = a,
        defaultLessThen = a,
        defaultLessThenEqual = a,
        defaultGreaterThen = a,
        defaultGreaterThenEqual = a,
        defaultMax = a,
        defaultMin = a,
        defaultToRational = a,
        defaultQuot = a,
        defaultRem = a,
        defaultDiv = a,
        defaultMod = a,
        defaultQuotRem = a,
        defaultDivMod = a,
        defaultToInteger = a,
        defaultDivide = a,
        defaultRecip = a,
        defaultFromRational = a,
        defaultFmap = a,
        defaultFconst = a,
        defaultPure = a,
        defaultAp = a,
        defaultLiftA2 = a,
        defaultDiscardLeft = a,
        defaultDiscardRight = a,
        defaultBind = a,
        defaultThen = a,
        defaultReturn = a,
        defaultFail = a
      }
  function <*> argument =
    Builtin
      { abort = abort function (abort argument),
        numInt = numInt function (numInt argument),
        numInteger = numInteger function (numInteger argument),
        enumBool = enumBool function (enumBool argument),
        enumChar = enumChar function (enumChar argument),
        enumInt = enumInt function (enumInt argument),
        enumInteger = enumInteger function (enumInteger argument),
        eqBool = eqBool function (eqBool argument),
        eqChar = eqChar function (eqChar argument),
        eqTuple = eqTuple function (eqTuple argument),
        eqInt = eqInt function (eqInt argument),
        eqInteger = eqInteger function (eqInteger argument),
        eqOrdering = eqOrdering function (eqOrdering argument),
        ordInt = ordInt function (ordInt argument),
        ordInteger = ordInteger function (ordInteger argument),
        functorList = functorList function (functorList argument),
        applicativeList = applicativeList function (applicativeList argument),
        monadList = monadList function (monadList argument),
        monadFailList = monadFailList function (monadFailList argument),
        defaultPlus = defaultPlus function (defaultPlus argument),
        defaultMinus = defaultMinus function (defaultMinus argument),
        defaultMultiply = defaultMultiply function (defaultMultiply argument),
        defaultNegate = defaultNegate function (defaultNegate argument),
        defaultAbs = defaultAbs function (defaultAbs argument),
        defaultSignum = defaultSignum function (defaultSignum argument),
        defaultFromInteger = defaultFromInteger function (defaultFromInteger argument),
        defaultSucc = defaultSucc function (defaultSucc argument),
        defaultPred = defaultPred function (defaultPred argument),
        defaultToEnum = defaultToEnum function (defaultToEnum argument),
        defaultFromEnum = defaultFromEnum function (defaultFromEnum argument),
        defaultEnumFrom = defaultEnumFrom function (defaultEnumFrom argument),
        defaultEnumFromThen = defaultEnumFromThen function (defaultEnumFromThen argument),
        defaultEnumFromTo = defaultEnumFromTo function (defaultEnumFromTo argument),
        defaultEnumFromThenTo = defaultEnumFromThenTo function (defaultEnumFromThenTo argument),
        defaultEqual = defaultEqual function (defaultEqual argument),
        defaultNotEqual = defaultNotEqual function (defaultNotEqual argument),
        defaultCompare = defaultCompare function (defaultCompare argument),
        defaultLessThen = defaultLessThen function (defaultLessThen argument),
        defaultLessThenEqual = defaultLessThenEqual function (defaultLessThenEqual argument),
        defaultGreaterThen = defaultGreaterThen function (defaultGreaterThen argument),
        defaultGreaterThenEqual = defaultGreaterThenEqual function (defaultGreaterThenEqual argument),
        defaultMax = defaultMax function (defaultMax argument),
        defaultMin = defaultMin function (defaultMin argument),
        defaultToRational = defaultToRational function (defaultToRational argument),
        defaultQuot = defaultQuot function (defaultQuot argument),
        defaultRem = defaultRem function (defaultRem argument),
        defaultDiv = defaultDiv function (defaultDiv argument),
        defaultMod = defaultMod function (defaultMod argument),
        defaultQuotRem = defaultQuotRem function (defaultQuotRem argument),
        defaultDivMod = defaultDivMod function (defaultDivMod argument),
        defaultToInteger = defaultToInteger function (defaultToInteger argument),
        defaultDivide = defaultDivide function (defaultDivide argument),
        defaultRecip = defaultRecip function (defaultRecip argument),
        defaultFromRational = defaultFromRational function (defaultFromRational argument),
        defaultFmap = defaultFmap function (defaultFmap argument),
        defaultFconst = defaultFconst function (defaultFconst argument),
        defaultPure = defaultPure function (defaultPure argument),
        defaultAp = defaultAp function (defaultAp argument),
        defaultLiftA2 = defaultLiftA2 function (defaultLiftA2 argument),
        defaultDiscardLeft = defaultDiscardLeft function (defaultDiscardLeft argument),
        defaultDiscardRight = defaultDiscardRight function (defaultDiscardRight argument),
        defaultBind = defaultBind function (defaultBind argument),
        defaultThen = defaultThen function (defaultThen argument),
        defaultReturn = defaultReturn function (defaultReturn argument),
        defaultFail = defaultFail function (defaultFail argument)
      }

instance Foldable Builtin where
  toList
    Builtin
      { abort,
        numInt,
        numInteger,
        enumBool,
        enumChar,
        enumInt,
        enumInteger,
        eqBool,
        eqChar,
        eqTuple,
        eqInt,
        eqInteger,
        eqOrdering,
        ordInt,
        ordInteger,
        functorList,
        applicativeList,
        monadList,
        monadFailList,
        defaultPlus,
        defaultMinus,
        defaultMultiply,
        defaultNegate,
        defaultAbs,
        defaultSignum,
        defaultFromInteger,
        defaultSucc,
        defaultPred,
        defaultToEnum,
        defaultFromEnum,
        defaultEnumFrom,
        defaultEnumFromThen,
        defaultEnumFromTo,
        defaultEnumFromThenTo,
        defaultEqual,
        defaultNotEqual,
        defaultCompare,
        defaultLessThen,
        defaultLessThenEqual,
        defaultGreaterThen,
        defaultGreaterThenEqual,
        defaultMax,
        defaultMin,
        defaultToRational,
        defaultQuot,
        defaultRem,
        defaultDiv,
        defaultMod,
        defaultQuotRem,
        defaultDivMod,
        defaultToInteger,
        defaultDivide,
        defaultRecip,
        defaultFromRational,
        defaultFmap,
        defaultFconst,
        defaultPure,
        defaultAp,
        defaultLiftA2,
        defaultDiscardLeft,
        defaultDiscardRight,
        defaultBind,
        defaultThen,
        defaultReturn,
        defaultFail
      } =
      [ abort,
        numInt,
        numInteger,
        enumBool,
        enumChar,
        enumInt,
        enumInteger,
        eqBool,
        eqChar,
        eqTuple,
        eqInt,
        eqInteger,
        eqOrdering,
        ordInt,
        ordInteger,
        functorList,
        applicativeList,
        monadList,
        monadFailList,
        defaultPlus,
        defaultMinus,
        defaultMultiply,
        defaultNegate,
        defaultAbs,
        defaultSignum,
        defaultFromInteger,
        defaultSucc,
        defaultPred,
        defaultToEnum,
        defaultFromEnum,
        defaultEnumFrom,
        defaultEnumFromThen,
        defaultEnumFromTo,
        defaultEnumFromThenTo,
        defaultEqual,
        defaultNotEqual,
        defaultCompare,
        defaultLessThen,
        defaultLessThenEqual,
        defaultGreaterThen,
        defaultGreaterThenEqual,
        defaultMax,
        defaultMin,
        defaultToRational,
        defaultQuot,
        defaultRem,
        defaultDiv,
        defaultMod,
        defaultQuotRem,
        defaultDivMod,
        defaultToInteger,
        defaultDivide,
        defaultRecip,
        defaultFromRational,
        defaultFmap,
        defaultFconst,
        defaultPure,
        defaultAp,
        defaultLiftA2,
        defaultDiscardLeft,
        defaultDiscardRight,
        defaultBind,
        defaultThen,
        defaultReturn,
        defaultFail
      ]
  foldMap go = foldMap go . toList

canonical :: Builtin Text
canonical =
  Builtin
    { abort = pack "abort",
      numInt = pack "numInt",
      numInteger = pack "numInteger",
      enumBool = pack "enumBool",
      enumChar = pack "enumChar",
      enumInt = pack "enumInt",
      enumInteger = pack "enumInteger",
      eqBool = pack "eqBool",
      eqChar = pack "eqChar",
      eqTuple = pack "eqTuple",
      eqInt = pack "eqInt",
      eqInteger = pack "eqInteger",
      eqOrdering = pack "eqOrdering",
      ordInt = pack "ordInt",
      ordInteger = pack "ordInteger",
      functorList = pack "functorList",
      applicativeList = pack "applicativeList",
      monadList = pack "monadList",
      monadFailList = pack "monadFailList",
      defaultPlus = pack "defaultPlus",
      defaultMinus = pack "defaultMinus",
      defaultMultiply = pack "defaultMultiply",
      defaultNegate = pack "defaultNegate",
      defaultAbs = pack "defaultAbs",
      defaultSignum = pack "defaultSignum",
      defaultFromInteger = pack "defaultFromInteger",
      defaultSucc = pack "defaultSucc",
      defaultPred = pack "defaultPred",
      defaultToEnum = pack "defaultToEnum",
      defaultFromEnum = pack "defaultFromEnum",
      defaultEnumFrom = pack "defaultEnumFrom",
      defaultEnumFromThen = pack "defaultEnumFromThen",
      defaultEnumFromTo = pack "defaultEnumFromTo",
      defaultEnumFromThenTo = pack "defaultEnumFromThenTo",
      defaultEqual = pack "defaultEqual",
      defaultNotEqual = pack "defaultNotEqual",
      defaultCompare = pack "defaultCompare",
      defaultLessThen = pack "defaultLessThen",
      defaultLessThenEqual = pack "defaultLessThenEqual",
      defaultGreaterThen = pack "defaultGreaterThen",
      defaultGreaterThenEqual = pack "defaultGreaterThenEqual",
      defaultMax = pack "defaultMax",
      defaultMin = pack "defaultMin",
      defaultToRational = pack "defaultToRational",
      defaultQuot = pack "defaultQuot",
      defaultRem = pack "defaultRem",
      defaultDiv = pack "defaultDiv",
      defaultMod = pack "defaultMod",
      defaultQuotRem = pack "defaultQuotRem",
      defaultDivMod = pack "defaultDivMod",
      defaultToInteger = pack "defaultToInteger",
      defaultDivide = pack "defaultDivide",
      defaultRecip = pack "defaultRecip",
      defaultFromRational = pack "defaultFromRational",
      defaultFmap = pack "defaultFmap",
      defaultFconst = pack "defaultFconst",
      defaultPure = pack "defaultPure",
      defaultAp = pack "defaultAp",
      defaultLiftA2 = pack "defaultLiftA2",
      defaultDiscardLeft = pack "defaultDiscardLeft",
      defaultDiscardRight = pack "defaultDiscardRight",
      defaultBind = pack "defaultBind",
      defaultThen = pack "defaultThen",
      defaultReturn = pack "defaultReturn",
      defaultFail = pack "defaultFail"
    }

builtin :: Builtin Text
unique :: [Text]
(builtin, unique) = case names of
  abort
    : numInt
    : numInteger
    : enumBool
    : enumChar
    : enumInt
    : enumInteger
    : eqBool
    : eqChar
    : eqTuple
    : eqInt
    : eqInteger
    : eqOrdering
    : ordInt
    : ordInteger
    : functorList
    : applicativeList
    : monadList
    : monadFailList
    : defaultPlus
    : defaultMinus
    : defaultMultiply
    : defaultNegate
    : defaultAbs
    : defaultSignum
    : defaultFromInteger
    : defaultSucc
    : defaultPred
    : defaultToEnum
    : defaultFromEnum
    : defaultEnumFrom
    : defaultEnumFromThen
    : defaultEnumFromTo
    : defaultEnumFromThenTo
    : defaultEqual
    : defaultNotEqual
    : defaultCompare
    : defaultLessThen
    : defaultLessThenEqual
    : defaultGreaterThen
    : defaultGreaterThenEqual
    : defaultMax
    : defaultMin
    : defaultToRational
    : defaultQuot
    : defaultRem
    : defaultDiv
    : defaultMod
    : defaultQuotRem
    : defaultDivMod
    : defaultToInteger
    : defaultDivide
    : defaultRecip
    : defaultFromRational
    : defaultFmap
    : defaultFconst
    : defaultPure
    : defaultAp
    : defaultLiftA2
    : defaultDiscardLeft
    : defaultDiscardRight
    : defaultBind
    : defaultThen
    : defaultReturn
    : defaultFail
    : unique -> (builtins, unique)
      where
        builtins =
          Builtin
            { abort,
              numInt,
              numInteger,
              enumBool,
              enumChar,
              enumInt,
              enumInteger,
              eqBool,
              eqChar,
              eqTuple,
              eqInt,
              eqInteger,
              eqOrdering,
              ordInt,
              ordInteger,
              functorList,
              applicativeList,
              monadList,
              monadFailList,
              defaultPlus,
              defaultMinus,
              defaultMultiply,
              defaultNegate,
              defaultAbs,
              defaultSignum,
              defaultFromInteger,
              defaultSucc,
              defaultPred,
              defaultToEnum,
              defaultFromEnum,
              defaultEnumFrom,
              defaultEnumFromThen,
              defaultEnumFromTo,
              defaultEnumFromThenTo,
              defaultEqual,
              defaultNotEqual,
              defaultCompare,
              defaultLessThen,
              defaultLessThenEqual,
              defaultGreaterThen,
              defaultGreaterThenEqual,
              defaultMax,
              defaultMin,
              defaultToRational,
              defaultQuot,
              defaultRem,
              defaultDiv,
              defaultMod,
              defaultQuotRem,
              defaultDivMod,
              defaultToInteger,
              defaultDivide,
              defaultRecip,
              defaultFromRational,
              defaultFmap,
              defaultFconst,
              defaultPure,
              defaultAp,
              defaultLiftA2,
              defaultDiscardLeft,
              defaultDiscardRight,
              defaultBind,
              defaultThen,
              defaultReturn,
              defaultFail
            }
  _ -> error "bad names"
