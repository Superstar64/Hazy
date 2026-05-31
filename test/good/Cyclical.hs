module Cyclical where

a = b

b = a

poly = (a 'a', a "a")

data Fix = Fix Fix

fix = Fix fix
