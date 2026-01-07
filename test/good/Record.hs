module Record where

data Custom

data Record = Record
  { a :: Int,
    b :: Char,
    c :: Custom
  }

match :: Record -> Char
match Record {b} = b

make :: Char -> Int -> Custom -> Record
make b a c = Record {b, a, c}

identity :: Record -> Record
identity Record {a, b, c} = Record {c, a, b}
