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
