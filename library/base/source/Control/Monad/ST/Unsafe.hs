module Control.Monad.ST.Unsafe where

import Control.Monad.ST (ST)
import Hazy (placeholder)

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = placeholder

unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST = placeholder

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = placeholder

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO = placeholder
