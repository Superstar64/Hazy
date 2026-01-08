{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef,
    modifyIORef',
  )
where

import System.IO (IO)
import Prelude (error)

data IORef a

newIORef :: a -> IO (IORef a)
newIORef = error "todo"

readIORef :: IORef a -> IO a
readIORef = error "todo"

writeIORef :: IORef a -> a -> IO ()
writeIORef = error "todo"

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef = error "todo"

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = error "todo"
