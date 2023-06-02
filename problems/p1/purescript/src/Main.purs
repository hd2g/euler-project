module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Array (range, filter)
import Data.Foldable (sum)

solve :: Int -> Int
solve m = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ range 0 (m - 1)

main :: Effect Unit
main = do
  log $ show $ solve 1000
