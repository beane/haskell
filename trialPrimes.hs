import Data.List

-- prime sieve!
filterPrimes :: [(Integer, Integer)] -> [(Integer, Integer)]
filterPrimes [] = []
filterPrimes list@((n,count):xs)
        | null list  = list
        | count == 0 = ((head list) : (filterPrimes candidates))
        | otherwise  = filterPrimes candidates
        where candidates = filter stillValid $ map incCount xs
              incCount (x,c) = if x `mod` n == 0
                              then (x,c+1)
                              else (x,c)
              stillValid (x,c) = c < 1

-- much sexier sieve
-- first number in list must be 2 (the first prime)
-- this isn't really safe as is
primeFilter :: [Integer] -> [Integer]
primeFilter [] = []
primeFilter list@(x:xs)
        | null list  = list
        | otherwise  = x : (primeFilter $ filter stillValid xs)
        where stillValid n = if n `mod` x == 0
                             then False
                             else True
primes :: [Integer]
primes = primeFilter [2..]

-- want this to handle infinite lists...
-- need to sort after
foldPrimes :: [Integer]
foldPrimes = foldl' step [2] [3,5..]
        where step acc el
                | (\x -> (el `mod` x) == 0) `any` acc = acc
                | otherwise = el : acc

