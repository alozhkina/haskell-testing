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
prop_sortedSingleElement :: (Ord a) => a -> Bool -> Bool
prop_sortedSingleElement x ascending = isSorted [x] ascending == True

-- 2. Если список состоит из двух элементов и булевый параметр True,
-- он отсортирован, если первый элемент меньше или равен второму
prop_sortedTwoElementsAscending :: (Ord a) => a -> a -> Bool
prop_sortedTwoElementsAscending x y = isSorted [x, y] True == (x <= y)

-- 3. Если список состоит из двух элементов и булевый параметр False,
-- он отсортирован, если первый элемент больше или равен второму
prop_sortedTwoElementsDescending :: (Ord a) => a -> a -> Bool
prop_sortedTwoElementsDescending x y = isSorted [x, y] False == (x >= y)


-- 4. Если список отсортирован, то удаление любого элемента не нарушает сортировку
prop_sortedAfterRemoval :: (Ord a) => [a] -> Bool -> Bool
prop_sortedAfterRemoval xs ascending =
   not (isSorted xs ascending) || all (\ys -> isSorted ys ascending) (removals xs)


removals :: [a] -> [[a]]
removals xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]


main :: IO ()
main = do
  putStrLn "Testing isCongruent"
  quickCheck prop_congruenceDifference
  quickCheck prop_congruenceSymmetry
  quickCheck prop_congruenceEquality
  putStrLn "Testing isSorted with int values"
  quickCheck (prop_sortedSingleElement :: Int -> Bool -> Bool)
  quickCheck (prop_sortedTwoElementsAscending :: Int -> Int -> Bool)
  quickCheck (prop_sortedTwoElementsDescending :: Int -> Int -> Bool)
  putStrLn "Testing isSorted with diffrent values"
  quickCheck (prop_sortedSingleElement :: (String, Int) -> Bool -> Bool)
  quickCheck (prop_sortedTwoElementsAscending :: (String, Int) -> (String, Int) -> Bool)
  quickCheck (prop_sortedTwoElementsDescending :: (String, Int) -> (String, Int) -> Bool)
  quickCheck (prop_sortedAfterRemoval :: [Float] -> Bool -> Bool)
