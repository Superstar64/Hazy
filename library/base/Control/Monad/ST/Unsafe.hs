module Control.Monad.ST.Unsafe where

import Control.Monad.ST (ST)

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = error "todo"

unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST = error "todo"

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = error "todo"

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO = error "todo"
