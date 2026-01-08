{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
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

data Ptr a

nullPtr :: Ptr a
nullPtr = error "todo"

castPtr :: Ptr a -> Ptr b
castPtr = error "todo"

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr = error "todo"

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr = error "todo"

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr = error "todo"

data FunPtr a

nullFunPtr :: FunPtr a
nullFunPtr = error "todo"

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr = error "todo"

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr = error "todo"

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr = error "todo"

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr = error "todo"

newtype IntPtr = IntPtr Int

ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr = error "todo"

intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr = error "todo"

newtype WordPtr = WordPtr Word

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr = error "todo"

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr = error "todo"
