module ArbitraryTest where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

trivial :: IO ()
trivial = sample trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = arbitrary >>= (\a -> return (Identity a))

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = arbitrary >>=
          (\a -> arbitrary >>=
                 (\b -> return (Pair a b)))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a
             | Second b 
             deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = arbitrary >>=
              (\a -> arbitrary >>=
                     (\b -> oneof [ return $ First a
                                  , return $ Second b]))

sumGenCharInt :: Gen (Sum Char Int)
-- sumGenCharInt = sumGenEqual
sumGenCharInt = sumGenFirstPls

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = arbitrary >>=
                 (\a -> arbitrary >>=
                        (\b -> frequency [ (10, return $ First a)
                                         , (3, return $ Second b)]))

