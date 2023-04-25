import Data.List (find)
import Control.Exception (assert)

factors :: Integral a => a -> [a]
-- factors n = case find (\i -> n `mod` i == 0) [2 .. floor . sqrt . fromIntegral $ n] of
factors n = case find ((== 0) . (n `mod`)) [2 .. floor . sqrt . fromIntegral $ n] of
  Just i -> i : factors (n `div` i)
  Nothing -> [n]

main :: IO ()
main = do
  !_ <- assert (factors 13195 == [5, 7, 13, 29]) $ pure ()
  !_ <- assert (factors 600851475143 == [71, 839, 1471, 6857]) $ pure ()
  print "ok"
