{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    WrappedMonad (..),
    WrappedArrow (..),
    ZipList (..),
    (<$>),
    (<$),
    (<**>),
    liftA,
    liftA3,
    optional,
    asum,
  )
where

import Data.Foldable (asum)
import Data.Functor (Functor, (<$), (<$>))
import Data.Maybe (Maybe)
import Prelude (error)

class (Functor f) => Applicative f where
  pure :: a -> f a
  infixl 4 <*>, *>, <*
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

class (Applicative f) => Alternative f where
  empty :: f a
  infixl 3 <|>
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

newtype Const a b = Const
  { getConst :: a
  }

newtype WrappedMonad m a = WrapMonad
  { unwrapMonad :: m a
  }

newtype WrappedArrow a b c = WrapArrow
  { unwrapArrow :: a b c
  }

newtype ZipList a = ZipList
  { getZipList :: [a]
  }

infixl 4 <**>

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = error "todo"

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA = error "todo"

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = error "todo"

optional :: (Alternative f) => f a -> f (Maybe a)
optional = error "todo"
