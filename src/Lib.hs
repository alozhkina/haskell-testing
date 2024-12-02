module Lib (isCongruent) where

-- Проверка равенства по модулю
isCongruent :: Int -> Int -> Int -> Bool
isCongruent a b m = (a - b) `mod` m == 0
