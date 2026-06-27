module Data.Functor.Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}
