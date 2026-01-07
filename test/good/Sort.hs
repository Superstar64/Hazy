module Sort where


sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy by = map runBy . sort . map By where
  import Data.List (sort)
  data By = By { runBy :: a }
  instance Eq By where
    a == b
      | EQ <- compare a b = True
      | otherwise = False
  instance Ord By where
    compare (By a) (By b) = by a b