module Chapter6Exercise where

import Data.List (sort)

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y


data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' = x == x' && y == y'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y)     = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _                           = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = x == x' && y == y'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = x == x' && y == y'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _                     = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y)     = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _                     = False


data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq, Ord)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
               then Blah
               else x

type Subject = String
type Verb    = String
type Object  = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


i :: Num a => a
i = 1
    
f :: Float
f = 1.0
    
f' :: Fractional a => a
f' = 1.0

f'' :: RealFrac a => a
f'' = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

sigmund' :: Int -> Int              -- Got it wrong
sigmund' x = myX

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char         -- Got it wrong Ord a => [a] -> a
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = f x