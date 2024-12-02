import Lib (isCongruent)
import Test.QuickCheck

-- 1. Если два числа равны по модулю, то их разность делится на модуль
prop_congruenceDifference :: Int -> Int -> Positive Int -> Bool
prop_congruenceDifference a b (Positive m) =
  isCongruent a b m == ((a - b) `mod` m == 0)

main :: IO ()
main = do
  putStrLn "Testing isCongruent"
  quickCheck prop_congruenceDifference