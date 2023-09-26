module P3

import Data.List

export
factors : Integer -> List Integer
factors n =
  case find ((== 0) . (n `mod`)) [2..(cast$ floor $ sqrt $ fromInteger n)] of
    Just i => i :: factors (n `div` i)
    Nothing => [n]
    
