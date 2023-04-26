import Data.List (find, (!!))
import Data.Maybe (isNothing)
import Control.Exception (assert)

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime n
  | even $ n `mod` 2 = False
  | otherwise = isNothing $ find ((== 0) . (n `mod`)) [3, 5 .. (floor . sqrt . fromIntegral) n]

primes = [x | x <- [2..], isPrime x]

main :: IO ()
main = do
  assert (primes !! 5 == 13) $ pure ()
  assert (primes !! 10000 == 104743) $ pure ()
  print "ok"
