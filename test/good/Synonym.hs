module Synonym where

type Synonym = String

type Identity a = a

str :: Identity Synonym
str = "abc"

type Partial = Either Int

right :: [Partial String]
right = [Left 1, Right "abc"]
