module Primes(primes) where

import Data.List

-- first number in list must be 2 (the first prime)
-- this isn't really safe as is
primeFilter :: [Integer] -> [Integer]
primeFilter [] = []
primeFilter list@(x:xs) = x : (primeFilter $ filter stillValid xs)
                        where stillValid n = if n `mod` x == 0
                                then False
                                else True

primes :: [Integer]
primes = primeFilter [2..]

