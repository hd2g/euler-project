module Main where

import Data.Maybe (isNothing)
import Data.List (find)

-- isPrime :: Integral a => a -> Bool
-- isPrime 2 = True
-- isPrime 3 = True
-- isPrime n
--   | even n =  False
--   | otherwise = isNothing $ find ((== 0) . mod n) [3, 5 .. floor $ sqrt $ fromIntegral n]
--
-- primes :: Integral a => [a]
-- primes = filter isPrime [2..]

-- https://qiita.com/ttatsf/items/510ec0cd4ad99fef9424
primes :: Integral a => [a]
primes = map fromIntegral $ [2, 3] ++ primes' where
  primes' = 5 : f 1 7 primes'
  f m s (p : ps) = [n | n <- ns, gcd m n == 1] ++ f (m * p) (p * p) ps where
    ns = [x + y | x <- [s, s + 6 .. p * p - 2], y <- [0, 4]]

-- |
-- >>> solve 10
-- 17
solve :: Integral a => a -> a
solve n = sum $ takeWhile (< n) primes

main :: IO ()
main = do
  print $ solve 2000000
