module Chapter7Exercise where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True

functionC x y =
  case x > y of
    True  -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True  -> n+2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0                     


tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ x `divMod` 10
        d     = xLast `mod` 10 

hunsD :: Integral a => a -> a
hunsD x = d
  where xLast = fst $ x `divMod` 100
        d     = xLast `mod` 10 

foldBool :: a -> a -> Bool -> a
foldBool x y z | z == True = x
               | otherwise = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z =
  case z of
    True  -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
