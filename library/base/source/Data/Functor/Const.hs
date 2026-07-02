module Data.Functor.Const where

newtype Const a b = Const {getConst :: a}
