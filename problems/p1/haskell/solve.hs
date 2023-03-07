module Main where

import Control.Exception (assert)

solve :: Int -> Int
solve n = sum [i | i <- [1..(n - 1)], i `mod` 3 == 0 || i `mod` 5 == 0]

main :: IO ()
main = do
  assert (solve 10 == 23) (pure ())
  assert (solve 1000 == 233168) (pure ())
