{-# LANGUAGE_HAZY NoStableImports, NoImplicitPrelude #-}

-- The compiler primitives are a combination of declarations that are hard wired
-- into the compiler from Hazy.Builtin and primitives that are defined in
-- userspace but left abstract.

-- |
-- This is the main module for accessing Hazy's internals. This module exports
-- all the compiler primitives and Hazy's internal Prelude.
module Hazy (module Hazy, module Hazy.Builtin, module Hazy.Helper, module Hazy.Prelude) where

import Hazy.Builtin
import Hazy.Helper
import Hazy.Prelude

-- The usage of 'missing' variable is deliberate. It's not a special compiler
-- builtin, but rather the compiler doesn't bother doing symbol resolution on
-- definitions it doesn't need.

placeholder :: a
placeholder = missing

error :: Text -> a
error = missing

pack :: [Char] -> Text
pack = missing

putStrLn :: Text -> IO ()
putStrLn = missing
