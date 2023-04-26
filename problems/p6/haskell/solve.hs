import Control.Exception (assert)

solve :: Int -> Int
solve n = sum ns ^ 2 - sum [n ^ 2 | n <- ns] where
  ns = [1..n]

main :: IO ()
main = do
  assert (solve 10 == 2640) $ pure ()
  assert (solve 100 == 25164150) $ pure ()
  print "ok"
