module Lib (isCongruent, isSorted) where

-- Проверка равенства по модулю
isCongruent :: Int -> Int -> Int -> Bool
isCongruent a b m = (a - b) `mod` m == 0

isSorted :: Ord a => [a] -> Bool -> Bool
isSorted [] _ = True -- пустой
isSorted [_] _ = True -- из одного
isSorted (x:y:xs) arg
  | arg == True = x <= y && isSorted (y:xs) arg
  | arg == False = x >= y && isSorted (y:xs) arg
