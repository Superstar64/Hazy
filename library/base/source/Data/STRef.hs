module Data.STRef
  ( STRef,
    newSTRef,
    readSTRef,
    writeSTRef,
    modifySTRef,
    modifySTRef',
  )
where

import Control.Monad.ST (ST)
import Hazy.Prelude (placeholder)

data STRef s a

newSTRef :: a -> ST s (STRef s a)
newSTRef = placeholder

readSTRef :: STRef s a -> ST s a
readSTRef = placeholder

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef = placeholder

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef = placeholder

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' = placeholder
