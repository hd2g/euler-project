module P1

export
solve : Int -> Int
solve n = sum [i | i <- [1..(n - 1)], i `mod` 3 == 0 || i `mod` 5 == 0]
