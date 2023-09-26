module P2

import Data.Colist

Foldable Colist where
  foldr _ a [] = a
  foldr f a (x :: xs) = foldr f (f x a) xs

fibs : Colist Integer
fibs = 0 :: scanl (+) 1 fibs

even : (Eq a, Integral a) => a -> Bool
even x = x `mod` 2 == 0

reverse : Colist a -> Colist a
reverse [] = []
reverse (x :: xs) = [x] `append` reverse xs

filter : (a -> Bool) -> Colist a -> Colist a
filter p xs = reverse $ filter' p xs where
  filter' : (a -> Bool) -> Colist a -> Colist a
  filter' _ [] = []
  filter' p (x :: xs) =
    if p x
    then x :: filter' p xs
    else filter' p xs

export
solve : Integer -> Integer
solve n = sum $ toList $ filter even $ takeWhile (<= n) fibs

