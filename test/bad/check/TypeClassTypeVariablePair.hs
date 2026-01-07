module TypeClassTypeVariablePair where

ambiguous :: (Eq (f Int), Eq (f Char)) => f Char -> Bool
ambiguous x = x == x
