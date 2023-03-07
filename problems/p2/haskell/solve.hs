module Main where

import Control.Exception (assert)

fibs = 0 : scanl (+) 1 fibs

solve :: Int -> Int
solve n = sum $ filter (\i -> i `mod` 2 == 0) $ takeWhile (\i -> i <= n) fibs

main :: IO ()
main = do
  assert (solve 4000000 == 4613732) (pure ())
