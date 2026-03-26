module Foreign.Ptr
  ( Ptr,
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,
    FunPtr,
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,
    freeHaskellFunPtr,
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr,
  )
where

import Hazy.Prelude (placeholder)

data Ptr a

nullPtr :: Ptr a
nullPtr = placeholder

castPtr :: Ptr a -> Ptr b
castPtr = placeholder

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr = placeholder

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr = placeholder

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr = placeholder

data FunPtr a

nullFunPtr :: FunPtr a
nullFunPtr = placeholder

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr = placeholder

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr = placeholder

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr = placeholder

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr = placeholder

newtype IntPtr = IntPtr Int

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr = placeholder

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr = placeholder

newtype WordPtr = WordPtr Word

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr = placeholder

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr = placeholder
