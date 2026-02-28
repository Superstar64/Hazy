{-# LANGUAGE_HAZY NoStableImports #-}
module Hazy (module Hazy.Builtin, module Hazy) where

import Hazy.Builtin

-- The usage of 'missing' variable is deliberate. It's not a special compiler
-- builtin, but rather the compiler doesn't bother doing symbol resolution on
-- definitions it doesn't need.

placeholder :: a
placeholder = missing

data Text

pack :: [Char] -> Text
pack = missing

data IO a

putStrLn :: Text -> IO ()
putStrLn = missing
