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
import Hazy.Prelude (placeholder)

data Builder

toLazyText :: Builder -> Lazy.Text
toLazyText = placeholder

toLazyTextWith :: Int -> Builder -> Lazy.Text
toLazyTextWith = placeholder

singleton :: Char -> Builder
singleton = placeholder

fromText :: Text -> Builder
fromText = placeholder

fromLazyText :: Lazy.Text -> Builder
fromLazyText = placeholder

fromString :: String -> Builder
fromString = placeholder

flush :: Builder
flush = placeholder
