{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeFixIO,
  )
where

import System.IO (IO)
import Prelude (error)

unsafePerformIO :: IO a -> a
unsafePerformIO = error "todo"

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = error "todo"

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = error "todo"

unsafeFixIO :: (a -> IO a) -> IO a
unsafeFixIO = error "todo"
