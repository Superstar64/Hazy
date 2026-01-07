module LocalImport where

sort :: Ord a => [a] -> [a]
sort = sort where
    import Data.List (sort)