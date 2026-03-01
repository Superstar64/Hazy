module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeFixIO,
  )
where

import Hazy (placeholder)
import System.IO (IO)
import Prelude (error)

unsafePerformIO :: IO a -> a
unsafePerformIO = placeholder

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = placeholder

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = placeholder

unsafeFixIO :: (a -> IO a) -> IO a
unsafeFixIO = placeholder
