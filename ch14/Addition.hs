module Addition where

import Test.QuickCheck
import Test.Hspec


sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count | n < d     = (count, n)
                     | otherwise = go (n - d) d (count + 1)


recMul :: Integral a => a -> a -> a
recMul mcant mlier = go mcant mlier 0
  where go mc ml result | ml == 0    = result
                        | otherwise  = go mc (ml - 1) (result + mc)
main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "5 multiplied by 4 is 20" $ do
      recMul 5 4 `shouldBe` 20
    it "12 multiplied by 0 is 0" $ do
      recMul 12 0 `shouldBe` 0
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
    it "dividedBy 5 has always remainder < 5" $ do
      property $ \x -> snd (dividedBy x (5::Integer)) < 5

oneThroughThree :: Gen Int
oneThroughThree = elements [1..100]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither =
  arbitrary >>=
  (\x -> arbitrary >>=
         (\y -> elements [Left x, Right y]))

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe =
  arbitrary >>=
  (\x -> elements [Nothing, Just x])
     
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' =
  arbitrary >>=
  (\x -> frequency [ (2, return Nothing)
                   , (3, return (Just x)) ])


propAdditionGreater :: Int -> Bool
propAdditionGreater x = x + 1 > x

runQC :: IO ()
runQC = quickCheck propAdditionGreater