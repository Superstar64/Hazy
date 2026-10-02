module Generate.Mangle.Builtin (Builtin (..), length, canonical, fromList) where

import Control.Applicative (liftA)
import Data.Foldable (Foldable (toList))
import qualified Data.Foldable as Foldable
import Data.Text (Text, pack)
import Prelude hiding (length)

data Builtin a = Builtin
  { abort,
    numInt,
    numInteger,
    numRatio,
    enumBool,
    enumChar,
    enumInt,
    enumInteger,
    enumOrdering,
    enumUnit,
    enumRatio,
    boundedBool,
    boundedChar,
    boundedInt,
    boundedOrdering,
    boundedTuple,
    eqBool,
    eqChar,
    eqTuple,
    eqInt,
    eqInteger,
    eqList,
    eqNonEmpty,
    eqOrdering,
    eqRatio,
    ordChar,
    ordTuple,
    ordInt,
    ordInteger,
    ordBool,
    ordList,
    ordNonEmpty,
    ordOrdering,
    ordRatio,
    realInt,
    realInteger,
    realRatio,
    integralInt,
    integralInteger,
    fractionalRatio,
    functorList,
    functorNonEmpty,
    applicativeList,
    applicativeNonEmpty,
    monadList,
    monadNonEmpty,
    monadFailList,
    functorST,
    applicativeST,
    monadST,
    semigroupArrow,
    semigroupList,
    semigroupNonEmpty,
    semigroupOrdering,
    semigroupST,
    semigroupTuple,
    monoidArrow,
    monoidList,
    monoidOrdering,
    monoidST,
    monoidTuple,
    showBool,
    showOrdering,
    showChar,
    showInt,
    showInteger,
    showTuple,
    showList,
    showNonEmpty,
    showRatio ::
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
      numRatio = pack "numRatio",
      enumBool = pack "enumBool",
      enumChar = pack "enumChar",
      enumInt = pack "enumInt",
      enumInteger = pack "enumInteger",
      enumOrdering = pack "enumOrdering",
      enumUnit = pack "enumUnit",
      enumRatio = pack "enumRatio",
      boundedBool = pack "boundedBool",
      boundedChar = pack "boundedChar",
      boundedInt = pack "boundedInt",
      boundedOrdering = pack "boundedOrdering",
      boundedTuple = pack "boundedTuple",
      eqBool = pack "eqBool",
      eqChar = pack "eqChar",
      eqTuple = pack "eqTuple",
      eqInt = pack "eqInt",
      eqNonEmpty = pack "eqNonEmpty",
      eqInteger = pack "eqInteger",
      eqList = pack "eqList",
      eqOrdering = pack "eqOrdering",
      eqRatio = pack "eqRatio",
      ordChar = pack "ordChar",
      ordTuple = pack "ordTuple",
      ordInt = pack "ordInt",
      ordInteger = pack "ordInteger",
      ordBool = pack "ordBool",
      ordList = pack "ordList",
      ordNonEmpty = pack "ordNonEmpty",
      ordOrdering = pack "ordOrdering",
      ordRatio = pack "ordRatio",
      realInt = pack "realInt",
      realInteger = pack "realInteger",
      realRatio = pack "realRatio",
      integralInt = pack "integralInt",
      integralInteger = pack "integralInteger",
      fractionalRatio = pack "fractionalRatio",
      functorList = pack "functorList",
      functorNonEmpty = pack "functorNonEmpty",
      applicativeList = pack "applicativeList",
      applicativeNonEmpty = pack "applicativeNonEmpty",
      monadList = pack "monadList",
      monadNonEmpty = pack "monadNonEmpty",
      monadFailList = pack "monadFailList",
      functorST = pack "functorST",
      applicativeST = pack "applicativeST",
      monadST = pack "monadST",
      semigroupArrow = pack "semigroupArrow",
      semigroupList = pack "semigroupList",
      semigroupNonEmpty = pack "semigroupNonEmpty",
      semigroupOrdering = pack "semigroupOrdering",
      semigroupST = pack "semigroupST",
      semigroupTuple = pack "semigroupTuple",
      monoidArrow = pack "monoidArrow",
      monoidList = pack "monoidList",
      monoidOrdering = pack "monoidOrdering",
      monoidST = pack "monoidST",
      monoidTuple = pack "monoidTuple",
      showBool = pack "showBool",
      showOrdering = pack "showOrdering",
      showChar = pack "showChar",
      showInt = pack "showInt",
      showInteger = pack "showInteger",
      showTuple = pack "showTuple",
      showList = pack "showList",
      showNonEmpty = pack "showNonEmpty",
      showRatio = pack "showRatio"
    }
