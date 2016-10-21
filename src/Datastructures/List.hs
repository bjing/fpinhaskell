module Datastructures.List where

import Control.Monad

-- 3.2
tail1 :: [a] -> [a]
tail1 [] = []
tail1 (x:xs) = xs

-- 3.3
setHead :: [a] -> a -> [a]
setHead xs newHead = newHead:xs

-- 3.4
drop1 :: [a] -> Int -> [a]
drop1 [] _ = []
drop1 xs 0 = xs
drop1 (x:xs) n = drop1 xs (n-1)

-- 3.5
dropWhile1 :: [a] -> (a -> Bool) -> [a]
dropWhile1 [] p = []
dropWhile1 (x:xs) p = if p x then dropWhile1 xs p else x:xs

-- 3.6
init1 :: [a] -> [a]
init1 [] = []
init1 [x] = []
init1 (x:xs) = x : init1 xs

-- 3.7
product1 :: Num a => [a] -> a
product1 = foldl (*) 1

-- 3.9
length1 :: [a] -> Int
length1 = foldr (\_ n -> n + 1) 0

-- 3.10
foldLeft :: [a] -> b -> (b -> a -> b) -> b
foldLeft [] acc f = acc
foldLeft (x:xs) acc f = foldLeft xs (f acc x) f

-- 3.11
sumFoldLeft :: Num a => [a] -> a
sumFoldLeft xs = foldLeft xs 0 (+)

productFoldLeft :: Num a => [a] -> a
productFoldLeft xs = foldLeft xs 1 (*)

lengthFoldLeft :: [a] -> Int
lengthFoldLeft xs = foldLeft xs 0 (\n _ -> n + 1)

-- 3.12
reverseFold :: [a] -> [a]
reverseFold = foldl (flip (:)) []

-- 3.13
-- foldRightViaFoldLeft is too hard, can't do
-- foldLeftViaFoldRight is too hard, can't do

-- 3.14
--appendFold ::

-- 3.15
concatViaFold :: [[a]] -> [a]
concatViaFold = foldl (++) []

-- 3.16
addOne :: Num a => [a] -> [a]
addOne = fmap (+1)
-- or
addOne1 :: Num a => [a] -> [a]
addOne1 [] = []
addOne1 (x:xs) = x + 1 : addOne1 xs

-- 3.17
doubleToString :: (Num a, Show a) => [a] -> [String]
doubleToString = fmap show

-- 3.18
map1 :: (a -> b) -> [a] -> [b]
map1 f (x:xs) = f x : map1 f xs

-- 3.19
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) = if p x then x : filter1 p xs else filter1 p xs

-- 3.20
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f ls = concat $ map f ls

-- 3.21
-- User flatMap to implement filter
--filterFlatMap :: (a -> Bool) -> [a] -> [a]
--filterFlatMap f xs = flatMap f xs

-- 3.22
combine :: Num a => [a] -> [a] -> [a]
combine [] x = x
combine x [] = x
combine (x:xs) (y:ys) = x + y : combine xs ys

-- 3.23
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f x [] = []
zipWith1 f [] y = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

-- 3.24
-- Solution 1 TODO implement filterM myself
hasSubsequence :: Eq a => [a] -> [a] -> Bool
hasSubsequence sup sub = sub `elem` subsequences sup

subsequences :: [a] -> [[a]]
subsequences = filterM (const [True, False])

-- Solution 2
hasSubsequence1 :: Eq a => [a] -> [a] -> Bool
hasSubsequence1 _ [] = True
hasSubsequence1 [] _ = False
hasSubsequence1 (x:xs) (y:ys)
  | x == y = hasSubsequence1 xs ys
  | otherwise = hasSubsequence1 xs (y:ys)
