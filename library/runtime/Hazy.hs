{-# LANGUAGE_HAZY NoStableImports #-}
module Hazy (module Hazy.Builtin, module Hazy) where

import Hazy.Builtin

placeholder :: a
placeholder = placeholder

data Text

pack :: [Char] -> Text
pack = placeholder

data IO a

putStrLn :: Text -> IO ()
putStrLn = placeholder
