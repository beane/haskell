-- naive approach to finding primes. takes about a minute around n=10K
notDivBy :: Integer -> Integer -> Bool
notDivBy a b = a `mod` b /= 0

findPrimes :: Integer -> [Integer]
findPrimes 0 = []
findPrimes 1 = [2]
findPrimes 2 = [2,3]
findPrimes n
  | n < 0     = findPrimes 0
  | otherwise = (prevPrimes ++ [nextPrime])
  where prevPrimes = findPrimes (n-1)
        firstCandidate  = last prevPrimes
        nextPrime = head [ candidate | candidate <- [firstCandidate+2,firstCandidate+4..], let notDivCand a = candidate `notDivBy` a ; in all notDivCand prevPrimes]
