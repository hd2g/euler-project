import Control.Exception (assert)
solve m = foldl lcm 1 [2..m]

main = do
  assert (solve 10 == 2520) $ pure ()
  assert (solve 20 == 232792560) $ pure ()
  print "ok"
