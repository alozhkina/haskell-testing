import Lib (isCongruent, isSorted)
import Test.QuickCheck

--isCongruent
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

--isSorted
-- 1. Если список состоит из одного элемента, он всегда отсортирован
prop_sortedSingleElement :: Int -> Bool -> Bool
prop_sortedSingleElement x ascending = isSorted [x] ascending == True

-- 2. Если список состоит из двух элементов и булевый параметр True,
-- он отсортирован, если первый элемент меньше или равен второму
prop_sortedTwoElementsAscending :: Int -> Int -> Bool
prop_sortedTwoElementsAscending x y =
  isSorted [x, y] True == (x <= y)

-- 3. Если список состоит из двух элементов и булевый параметр False,
-- он отсортирован, если первый элемент больше или равен второму
prop_sortedTwoElementsDescending :: Int -> Int -> Bool
prop_sortedTwoElementsDescending x y =
  isSorted [x, y] False == (x >= y)



main :: IO ()
main = do
  putStrLn "Testing isCongruent"
  quickCheck prop_congruenceDifference
  quickCheck prop_congruenceSymmetry
  quickCheck prop_congruenceEquality
  putStrLn "Testing isSorted"
  quickCheck prop_sortedSingleElement
  quickCheck prop_sortedTwoElementsAscending
  quickCheck prop_sortedTwoElementsDescending