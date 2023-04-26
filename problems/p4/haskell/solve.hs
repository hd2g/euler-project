import Control.Exception (assert)

splitAsPalindromic :: String -> (String, String)
splitAsPalindromic s = trunc . splitAt pos $ s where
  size = length s
  pos = size `div` 2
  trunc :: (String, String) -> (String, String)
  trunc (a,b) = (a, cut b) where
    cut
      | even size = id
      | otherwise = drop 1

isPalindromic :: String -> Bool
isPalindromic = judge . splitAsPalindromic where
  judge (a,b) = a == reverse b

solve :: Int -> [Integer]
solve digit = [x * y | x <- [1..m], y <- [1..m], isPalindromic . show $ x * y] where
  m =  10 ^ digit - 1

main :: IO ()
main = do
  assert ((maximum . solve) 2 == 9009) $ pure ()
  assert ((maximum . solve) 3 == 906609) $ pure ()
  print "ok"
