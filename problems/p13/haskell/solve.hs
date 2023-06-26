module P13 where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR

import Data.Either (rights)

dat :: IO [Integer]
dat = do
  dat' <- TIO.readFile "../dat"
  pure $ map fst $ rights $ map TR.decimal $ T.lines dat'

-- |
-- TODO: 多倍長整数同しの加算を実装してみたい
--
add :: Integral a => [a] -> a
add = sum

solve :: Integral a => IO [a] -> IO a
solve xs = add <$> xs
