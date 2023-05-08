{-# language OverloadedStrings #-}
{-# language TupleSections #-}

import Prelude hiding (lines, readFile)
import Data.Text qualified as T
import Data.Text (Text, splitOn, lines)
import Data.Text.IO (readFile)

type Dat = [((Int, Int), Int)]

size :: Int
size = 4

lists' :: Dat -> [(Int, Int)] -> Dat
lists' _ [] = []
lists' dat ((y, x) : tl) =
  filter (\ ((y', x'), _) -> y == y' && x == x') dat ++ lists' dat tl

lists :: Dat -> [(Int, Int)] -> [Integer]
lists dat yxs = map (toInteger . snd) $ lists' dat yxs

listToRight :: Dat -> (Int, Int) -> [Integer]
listToRight dat (y, x) = lists dat $ map (y,) [x .. x + size]

listToDown :: Dat -> (Int, Int) -> [Integer]
listToDown dat (y, x) = lists dat $ map (,x) [y .. y + size]

listToDiagonal :: Dat -> (Int, Int) -> [Integer]
listToDiagonal dat (y, x) = lists dat $ zip [x .. x + size] [y .. y + size]

solve :: Dat -> Integer
solve dat = maximum $ map product ls where
  ls :: [[Integer]]
  ls = do
    y <- [0 .. 20]
    x <- [0 .. 20]
    [listToRight dat (y, x), listToDown dat (y, x), listToDiagonal dat (y, x)]

withAxis :: [Int] -> [Int] -> [[Int]] -> Dat
withAxis ys xs vss = do
  y <- ys
  x <- xs
  v <- concat vss
  return ((y, x), v)

-- datFromFile :: FilePath -> IO [[Int]]
datFromFile :: FilePath -> IO Dat
datFromFile pathname = do
  content <- readFile pathname 
  return $ withAxis [0..19] [0..19] $ map (map parseInt . splitOn " ") . lines $ content where
    parseInt :: Text -> Int
    parseInt = read . T.unpack

main :: IO ()
main = undefined
