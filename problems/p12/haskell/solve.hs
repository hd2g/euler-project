module P12 where

import Data.List (scanl, find, group)

triangles :: [Int]
triangles = scanl (+) 0 [1..]

divisors :: Int -> [Int]
divisors n = case find (\i -> n `mod` i == 0) [2 .. floor $ sqrt $ fromIntegral n] of
  Just i -> i : divisors (n `div` i)
  Nothing -> [n]

countOfDivisors :: Int -> Int
countOfDivisors = product . map (\gs -> length gs + 1) . group . divisors

-- |
-- >>> solve 500
-- Just 76576500
solve :: Int -> Maybe Int
solve n = fst <$> (find (\(x, c) -> n <= c) $ zip triangles $ map countOfDivisors triangles)
