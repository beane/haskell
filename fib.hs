-- generate an infinite list of fibs...
fibHelper :: Integer -> Integer -> [Integer]
fibHelper a b
    | a > b          = fibHelper b a
    | a < 0 || b < 0 = []
    | otherwise      = a:(fibHelper b nextNum)
      where nextNum  = a + b

fib :: [Integer]
fib = fibHelper 0 1
