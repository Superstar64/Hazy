module Stage5.Generate.Mangle
  ( module Stage5.Generate.Mangle,
    Builtin (..),
    canonical,
  )
where

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
import Stage5.Generate.Mangle.Builtin (Builtin (..), canonical)
import qualified Stage5.Generate.Mangle.Builtin as Builtin
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
      Type2.Lazy -> fromString "Hazy.Lazy"
      Type2.Strict -> fromString "Hazy.Strict"
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

builtin :: Builtin Text
builtin = Builtin.fromList $ take Builtin.length names

unique :: [Text]
unique = drop Builtin.length names
