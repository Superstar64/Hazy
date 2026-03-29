module Stage5.Generate.Mangle.Builtin (Builtin (..), length, canonical, fromList) where

import Control.Applicative (liftA)
import Data.Foldable (Foldable (toList))
import qualified Data.Foldable as Foldable
import Data.Text (Text, pack)
import Prelude hiding (length)

data Builtin a = Builtin
  { abort,
    numInt,
    numInteger,
    enumBool,
    enumChar,
    enumInt,
    enumInteger,
    enumOrdering,
    enumUnit,
    eqBool,
    eqChar,
    eqTuple,
    eqInt,
    eqInteger,
    eqList,
    eqOrdering,
    ordChar,
    ordTuple,
    ordInt,
    ordInteger,
    ordBool,
    ordList,
    ordOrdering,
    realInt,
    realInteger,
    integralInt,
    integralInteger,
    functorList,
    applicativeList,
    monadList,
    monadFailList,
    functorST,
    applicativeST,
    monadST,
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
  deriving (Functor, Foldable, Traversable)

{-# UnorderedRecords Builtin #-}

instance Applicative Builtin where
  pure a = fromList (replicate length a)
  function <*> argument = fromList $ zipWith id (toList function) (toList argument)

length = Foldable.length (toList canonical)

newtype State x a = State {runState :: [x] -> (a, [x])}

instance Functor (State x) where
  fmap = liftA

instance Applicative (State x) where
  pure a = State $ \xs -> (a, xs)
  State f <*> State x = State $ \xs -> case f xs of
    (f, xs) -> case x xs of
      (x, xs) -> (f x, xs)

next :: State x x
next = State $ \xs -> (head xs, tail xs)

fromList :: [x] -> Builtin x
fromList list = fst $ runState (traverse (const next) canonical) list

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
      enumOrdering = pack "enumOrdering",
      enumUnit = pack "enumUnit",
      eqBool = pack "eqBool",
      eqChar = pack "eqChar",
      eqTuple = pack "eqTuple",
      eqInt = pack "eqInt",
      eqInteger = pack "eqInteger",
      eqList = pack "eqList",
      eqOrdering = pack "eqOrdering",
      ordChar = pack "ordChar",
      ordTuple = pack "ordTuple",
      ordInt = pack "ordInt",
      ordInteger = pack "ordInteger",
      ordBool = pack "ordBool",
      ordList = pack "ordList",
      ordOrdering = pack "ordOrdering",
      realInt = pack "realInt",
      realInteger = pack "realInteger",
      integralInt = pack "integralInt",
      integralInteger = pack "integralInteger",
      functorList = pack "functorList",
      applicativeList = pack "applicativeList",
      monadList = pack "monadList",
      monadFailList = pack "monadFailList",
      functorST = pack "functorST",
      applicativeST = pack "applicativeST",
      monadST = pack "monadST",
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
