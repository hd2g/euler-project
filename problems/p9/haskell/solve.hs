import Data.List (find, concatMap)
import Control.Exception (assert)

pythagoreans :: Integer -> [(Integer, Integer, Integer)]
pythagoreans m = [(m * m - n * n, 2 * m * n, m * m + n * n) | n <- [m - 1, m - 2 .. 1]]

solve' :: Integer -> [Integer]
solve' summary = case find (\ (a,b,c) -> a + b + c == summary) $ concatMap pythagoreans [2..] of
  Just (a,b,c) -> [a,b,c]
  Nothing -> []

solve = product . solve'

main :: IO ()
main = do
  assert (solve 12 == 60) $ pure ()
  assert (solve 1000 == 31875000) $ pure ()
