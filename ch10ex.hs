module Chapter10Exercise where

import Data.Bool

stops  = "pbtdkg"
vowels = "aeiou"

nouns = ["nurse", "cat", "party", "oil", "poverty"]
verbs = ["run", "look", "feel", "walk", "talk"]

words3 :: [a] -> [a] ->[(a, a, a)]
words3 c v = concat . concat $ map (\x -> map (\y -> map (\z -> (x, y, z)) c) v) c

pwords3 :: [(Char, Char, Char)]
pwords3 = filter (\(x, _, _) -> x == 'p') $ words3 stops vowels


seekritFunc x = 
  div (sum (map length (words x))) (length (words x))

seekritFuncFrac x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) | f x == True = True
               | otherwise   = myAny f xs
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x _ -> f x == True) False

-- myFr f z []     = z
-- myFr f z (x:xs) = f x $ foldr f z xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y z -> bool z True (x == y)) False

myReverse :: [a] -> [a]
myReverse = foldr (\x y -> (y++[x])) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> (f x:y)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr f []
  where f x y | p x == True = (x:y)
              | otherwise   = y

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 g 
  where g x y | f x y == GT = x
              | f x y == EQ = x
              | otherwise   = y


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
