{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

data STRef s a

newSTRef :: a -> ST s (STRef s a)
newSTRef = error "todo"

readSTRef :: STRef s a -> ST s a
readSTRef = error "todo"

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef = error "todo"

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef = error "todo"

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' = error "todo"
