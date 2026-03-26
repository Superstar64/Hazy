module Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef,
    modifyIORef',
  )
where

import Hazy.Prelude (placeholder)
import System.IO (IO)
import Prelude (error)

data IORef a

newIORef :: a -> IO (IORef a)
newIORef = placeholder

readIORef :: IORef a -> IO a
readIORef = placeholder

writeIORef :: IORef a -> a -> IO ()
writeIORef = placeholder

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef = placeholder

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = placeholder
