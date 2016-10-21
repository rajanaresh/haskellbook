module Chapter14Exercise where

import Test.QuickCheck

import Data.List (sort)
import Data.Bool (bool)

half :: Float -> Float
half x = x/2

halfIdentity :: Float -> Float
halfIdentity = (*2) . half

halfTest :: IO ()
halfTest = quickCheck (\x -> halfIdentity x == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)
        
listTestString :: IO ()
listTestString = quickCheck (\x -> listOrdered (sort (x::[String])) == True)
listTestChar :: IO ()
listTestChar = quickCheck (\x -> listOrdered (sort (x::[Char])) == True)
listTestInt :: IO ()
listTestInt = quickCheck (\x -> listOrdered (sort (x::[Int])) == True)
listTestIntString :: IO ()
listTestIntString = quickCheck (\x -> listOrdered (sort (x::[(Int, String)])) == True)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
  
testPlusAssociative :: IO ()
testPlusAssociative = quickCheck (\x y z -> plusAssociative (x::Integer) (y::Integer) (z::Integer))
testPlusCommutative :: IO ()
testPlusCommutative = quickCheck (\x y -> plusCommutative (x::Integer) (y::Integer))

mulAssociative x y z = x * (y * z) == (x * y) * z
mulCommutative x y = x * y == y * x

testMulAssociative :: IO ()
testMulAssociative = quickCheck (\x y z -> mulAssociative (x::Integer) (y::Integer) (z::Integer))
testMulCommutative :: IO ()
testMulCommutative = quickCheck (\x y -> mulCommutative (x::Integer) (y::Integer))

quotRemLaw1 x y = (quot x y) * y + (rem x y) == x
quotRemLaw2 x y= (div x y) * y + (mod x y) == x

testQuotRemLaw1 :: IO ()
testQuotRemLaw1 = quickCheck (\x y -> bool (quotRemLaw1 (x::Integer) (y::Integer)) True (y == 0))
testQuotRemLaw2 :: IO ()
testQuotRemLaw2 = quickCheck (\x y -> bool (quotRemLaw2 (x::Integer) (y::Integer)) True (y == 0))


powerAssociative x y z = x^(y^z) == (x^y)^z
testPowerAssociative :: IO ()
testPowerAssociative = quickCheck (\x y z -> powerAssociative  (x::Integer) (y::Integer) (z::Integer))

reverseList x = id x == (reverse . reverse) x 
testReverseList :: IO ()
testReverseList = quickCheck (\x -> reverseList (x::[Integer]))

main :: IO ()
main = halfTest >>
       listTestString >>
       listTestChar >>
       listTestInt >>
       listTestIntString >>
       testPlusAssociative >>
       testPlusCommutative >>
       testMulAssociative >>
       testMulCommutative >>
       testPowerAssociative >>
       testQuotRemLaw1 >>
       testQuotRemLaw2 >>
       testReverseList
       
