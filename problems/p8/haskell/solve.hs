import Data.Char (digitToInt)
import Control.Exception (assert)

type Line = String
type Dat = [Line]
type Digit = Int

solve'' :: Line -> Digit -> [Integer]
solve'' line digit
  | length line < digit = []
  | otherwise = (toInteger . product . map digitToInt . take digit $ line) : solve'' (drop 1 line) digit

solve' :: Dat -> Digit -> [Integer]
solve' [] _ = []
solve' (line:tl) digit = solve'' line digit ++ solve' tl digit

solve :: Dat -> Digit -> Integer
solve dat = maximum . solve' dat

main :: IO ()
main = do
  dat <- lines <$> readFile "../dat"
  assert (solve dat 4 == 5832) $ pure ()
  assert (solve dat 13 == 5377010688) $ pure ()
