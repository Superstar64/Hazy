module Hazy.PreludeList where

import Hazy.Prelude

infixl 9 !!

infixr 5 ++

infix 4 `elem`, `notElem`

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =
  q
    : ( case xs of
          [] -> []
          x : xs -> scanl f (f q x) xs
      )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

class Foldable t where
  fold :: (Monoid m) => t m -> m
  fold = placeholder

  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldMap = placeholder

  foldMap' :: (Monoid m) => (a -> m) -> t a -> m
  foldMap' = placeholder

  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr = placeholder

  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f z = foldl f z . toList
    where
      foldl :: (a -> b -> a) -> a -> [b] -> a
      foldl f z [] = z
      foldl f z (x : xs) = foldl f (f z x) xs
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldl' = placeholder

  foldr1 :: (a -> a -> a) -> t a -> a
  foldr1 f = foldr1 f . toList
    where
      foldr1 :: (a -> a -> a) -> [a] -> a
      foldr1 f [x] = x
      foldr1 f (x : xs) = f x (foldr1 f xs)
      foldr1 _ [] = error "Prelude.foldr1: empty list"

  foldl1 :: (a -> a -> a) -> t a -> a
  foldl1 f = foldl1 f . toList
    where
      foldl1 :: (a -> a -> a) -> [a] -> a
      foldl1 f (x : xs) = foldl f x xs
      foldl1 _ [] = error "Prelude.foldl1: empty list"

  toList :: t a -> [a]
  toList = foldr (:) []

  null :: t a -> Bool
  null = null . toList
    where
      null :: [a] -> Bool
      null [] = True
      null (_ : _) = False

  length :: t a -> Int
  length = length . toList
    where
      length :: [a] -> Int
      length [] = 0
      length (_ : l) = 1 + length l

  elem :: (Eq a) => a -> t a -> Bool
  elem x = elem x . toList
    where
      elem :: (Eq a) => a -> [a] -> Bool
      elem x = any (== x)

  maximum :: (Ord a) => t a -> a
  maximum = maximum . toList
    where
      maximum :: (Ord a) => [a] -> a
      maximum [] = error "Prelude.maximum: empty list"
      maximum xs = foldl1 max xs

  minimum :: (Ord a) => t a -> a
  minimum = minimum . toList
    where
      minimum :: (Ord a) => [a] -> a
      minimum [] = error "Prelude.minimum: empty list"
      minimum xs = foldl1 min xs

  sum :: (Num a) => t a -> a
  sum = sum . toList
    where
      sum :: (Num a) => [a] -> a
      sum = foldl (+) 0

  product :: (Num a) => t a -> a
  product = product . toList
    where
      product :: (Num a) => [a] -> a
      product = foldl (*) 1

instance Foldable [] where
  foldr f z [] = z
  foldr f z (x : xs) = f x (foldr f z xs)

  toList = id

instance Foldable NonEmpty where
  foldr f z (x :| xs) = f x (foldr f z xs)
  toList (x :| xs) = x : xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x : xs')
  | p x = (x : ys, zs)
  | otherwise = ([], xs)
  where
    (ys, zs) = span p xs'
break p = span (not . p)

lines :: String -> [String]
lines "" = []
lines s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines s''

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s''
    where
      (w, s'') = break isSpace s'

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ' : s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: (Foldable t) => t Bool -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (Foldable t) => (a -> Bool) -> t a -> Bool
any p = or . map p . toList
all p = and . map p . toList

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x xs = not (elem x xs)

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x, y) : xys)
  | key == x = Just y
  | otherwise = lookup key xys

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a : as) (b : bs) =
  z a b : zipWith z as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a : as) (b : bs) (c : cs) =
  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr
    (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs))
    ([], [], [])
