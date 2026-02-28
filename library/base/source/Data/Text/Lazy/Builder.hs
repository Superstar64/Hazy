module Data.Text.Lazy.Builder
  ( Builder,
    toLazyText,
    toLazyTextWith,
    singleton,
    fromText,
    fromLazyText,
    fromString,
    flush,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)

data Builder

toLazyText :: Builder -> Lazy.Text
toLazyText = error "todo"

toLazyTextWith :: Int -> Builder -> Lazy.Text
toLazyTextWith = error "todo"

singleton :: Char -> Builder
singleton = error "todo"

fromText :: Text -> Builder
fromText = error "todo"

fromLazyText :: Lazy.Text -> Builder
fromLazyText = error "todo"

fromString :: String -> Builder
fromString = error "todo"

flush :: Builder
flush = error "todo"
