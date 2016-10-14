module Chapter9Exercise where

import Data.Char
import Data.Bool

  
stringToWords' :: String -> [String]
stringToWords' [] = []
stringToWords' ss = (x : stringToWords xs)
  where x  = takeWhile (/=' ') ss
        xs = dropWhile (==' ') $ dropWhile (/=' ') ss

splitWith :: (Eq a) => a -> [a] -> [[a]]
splitWith _ [] = []
splitWith c ss = (x : splitWith c xs)
  where x  = takeWhile (/=c) ss
        xs = dropWhile (==c) $ dropWhile (/=c) ss
            
stringToWords :: String -> [String]
stringToWords = splitWith ' '

multipleOf3 :: Integral a => [a] -> [a]
multipleOf3 = filter (\x -> rem x 3 == 0)

lengthOf3 :: Integral a => [a] -> Int
lengthOf3 = length . multipleOf3

myFilter :: String -> [String]
myFilter = filter (\x -> not $ elem x ["the", "an", "a"]) . words


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = ((x,y) : myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ []          = []
myZipWith _ [] _          = []
myZipWith f (x:xs) (y:ys) = (x `f` y : myZipWith f xs ys)

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

allCapitals :: String -> String
allCapitals = filter isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x:xs)

capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (x:xs) = (toUpper x: capitalizeAll xs)

capitalizeAll' :: String -> String
capitalizeAll' = map toUpper

capitalize' :: String -> Char
capitalize' = toUpper . head

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = bool (myAny f xs) (f x) (f x)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny' (==e)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = reverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap ::(a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:y:[]) | f x y == GT = x
                       | f x y == LT = y
                       | otherwise   = x
myMaximumBy f (x:y:xs) | f x y == GT = myMaximumBy f (x:xs)
                       | f x y == LT = myMaximumBy f (y:xs)
                       | otherwise   = myMaximumBy f (x:xs)


myMinimumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:y:[]) | f x y == GT = y
                       | f x y == LT = x
                       | otherwise   = y
myMinimumBy f (x:y:xs) | f x y == GT = myMinimumBy f (y:xs)
                       | f x y == LT = myMinimumBy f (x:xs)
                       | otherwise   = myMinimumBy f (y:xs)


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare            

