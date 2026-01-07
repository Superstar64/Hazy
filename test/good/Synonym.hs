module Synonym where

type Synonym = String

type Identity a = a

str :: Identity Synonym
str = "abc"
