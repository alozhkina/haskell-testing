import Lib (isCongruent)
import Test.QuickCheck

-- 1. Если два числа равны по модулю, то их разность делится на модуль
prop_congruenceDifference :: Int -> Int -> Positive Int -> Bool
prop_congruenceDifference a b (Positive m) =
  isCongruent a b m == ((a - b) `mod` m == 0)

-- 2. Равенство по модулю является симметричным
prop_congruenceSymmetry :: Int -> Int -> Positive Int -> Bool
prop_congruenceSymmetry a b (Positive m) =
  isCongruent a b m == isCongruent b a m

-- 3. Если одно число равно другому, то они равны по любому модулю
prop_congruenceEquality :: Int -> Positive Int -> Bool
prop_congruenceEquality a (Positive m) =
  isCongruent a a m == True

main :: IO ()
main = do
  putStrLn "Testing isCongruent"
  quickCheck prop_congruenceDifference
  quickCheck prop_congruenceSymmetry
  quickCheck prop_congruenceEquality