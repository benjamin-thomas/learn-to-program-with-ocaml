import Data.List (unfoldr)

-- echo ./sieve.hs | entr -c doctest /_

{- |
>>> multipleOf 3 12
True

>>> multipleOf 3 13
False
-}
multipleOf :: (Integral a) => a -> a -> Bool
multipleOf x' n = n `mod` x' == 0

{- |
>>> sieve []
[]

>>> sieve [1..25]
[2,3,5,7,11,13,17,19,23]
-}
sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (x : xs)
  | x < 0 = error "bad input"
  | x < 2 = sieve xs
  | otherwise = x : sieve (filter (not . multipleOf x) xs)

{- |

>>> sieve' 25
[2,3,5,7,11,13,17,19,23]

>>> sieve' 42
[2,3,5,7,11,13,17,19,23,29,31,37,41]
-}
sieve' :: (Integral a) => a -> [a]
sieve' n = sieve [1 .. n]

{- | From https://www.haskell.org
>>> take 9 primes
[2,3,5,7,11,13,17,19,23]

>>> take 9 primes'
[2,3,5,7,11,13,17,19,23]
-}
primes :: [Integer]
primes = filterPrime [2 ..]
 where
  filterPrime (p : xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]

primes' :: [Integer]
primes' = unfoldr nextPrime [2 ..]
 where
  nextPrime (p : xs) = Just (p, filter (\x -> x `mod` p /= 0) xs)
