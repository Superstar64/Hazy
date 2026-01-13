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
import System.FilePath ((</>))

names :: [Text]
names = filter (`Set.notMember` keywords) $ map pack $ go [[]]
  where
    go base =
      let prefix = base >>= (\base -> [letter : base | letter <- ['a' .. 'z']])
       in prefix ++ go prefix

data Brand
  = Class
  | Data

mangle :: Variable -> Text
mangle = \case
  VariableIdentifier name -> runVariableIdentifier name
  VariableSymbol name -> runVariableSymbol name

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
      Type2.Num -> fromString "Hazy.Num"
      Type2.Enum -> fromString "Hazy.Enum"
      Type2.Eq -> fromString "Hazy.Eq"
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

-- todo merge into field
supers :: [Text]
supers = [pack ("$" ++ show i) | i <- [0 ..]]

fields :: [Text]
fields = names

path :: FullQualifiers -> FilePath
path = (++ ".mjs") . foldr1 (</>) . fmap unpack . path'

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
    enumInt,
    enumInteger,
    eqBool,
    eqChar,
    eqInt,
    eqInteger ::
      a
  }

instance Functor Builtin where
  fmap = liftA

instance Applicative Builtin where
  pure abort@numInt@numInteger@enumInt@enumInteger@eqBool@eqChar@eqInt@eqInteger =
    Builtin
      { abort,
        numInt,
        numInteger,
        enumInt,
        enumInteger,
        eqBool,
        eqChar,
        eqInt,
        eqInteger
      }
  function <*> argument =
    Builtin
      { abort = abort function (abort argument),
        numInt = numInt function (numInt argument),
        numInteger = numInteger function (numInteger argument),
        enumInt = enumInt function (enumInt argument),
        enumInteger = enumInteger function (enumInteger argument),
        eqBool = eqBool function (eqBool argument),
        eqChar = eqChar function (eqChar argument),
        eqInt = eqInt function (eqInt argument),
        eqInteger = eqInteger function (eqInteger argument)
      }

instance Foldable Builtin where
  toList Builtin {abort, numInt, numInteger, enumInt, enumInteger} =
    [ abort,
      numInt,
      numInteger,
      enumInt,
      enumInteger
    ]
  foldMap go = foldMap go . toList

canonical :: Builtin Text
canonical =
  Builtin
    { abort = pack "abort",
      numInt = pack "numInt",
      numInteger = pack "numInteger",
      enumInt = pack "enumInt",
      enumInteger = pack "enumInteger",
      eqBool = pack "eqBool",
      eqChar = pack "eqChar",
      eqInt = pack "eqInt",
      eqInteger = pack "eqInteger"
    }

builtin :: Builtin Text
unique :: [Text]
(builtin, unique) = case names of
  abort
    : numInt
    : numInteger
    : enumInt
    : enumInteger
    : eqBool
    : eqChar
    : eqInt
    : eqInteger
    : unique -> (builtins, unique)
      where
        builtins =
          Builtin
            { abort,
              numInt,
              numInteger,
              enumInt,
              enumInteger,
              eqBool,
              eqChar,
              eqInt,
              eqInteger
            }
  _ -> error "bad names"
