-- {-# OPTIONS -Wall #-}

module Chapter11Exercise where

import Data.Char
import Data.Bool
import Data.List 

data Price = Price Integer
             deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir Integer
             | CatapultsR'Us Integer
             | TakeYourChancesUnited Integer
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane (PapuAir 10)
                      
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> Bool
areCars = foldr ((&&) . isCar) True

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

data Example = MakeExample deriving Show
data Example' = MakeExample' Int deriving Show


data Id a = MkId a deriving (Eq, Show)
idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show, Enum)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | Purescript
                         deriving (Eq, Show, Enum)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                  deriving (Eq, Show)


allProgrammers = [ Programmer x y | x <- enumFrom GnuPlusLinux , y <- enumFrom Haskell ]

ceasar :: Int -> String -> String
ceasar n = toChars . encode . toInts
  where toInts  = map ord
        encode  = map (\x -> bool (lower x) (upper x) (isUpper x))
          where upper x   = 65 + ((n + x - 65) `mod` 26)
                lower x   = 97 + ((n + x - 97) `mod` 26)
                isUpper x = x < 97
        toChars = map chr

unceasar :: Int -> String -> String
unceasar n = ceasar (-n)

vigenere :: String -> String -> String
vigenere k t  = concat $ map (\x -> ceasar (shift $ snd x) [fst x]) input
  where key                 = take (length t) $ concat $ repeat k
        input               = zip t key 
        shift x | isUpper x = ord x - 65
                | otherwise = ord x - 97


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _                                        = True
isSubsequenceOf' _ []                                        = False
isSubsequenceOf' s@(sx:sxs) t@(tx:txs) | length s > length t = False
                                      | sx == tx            = True && isSubsequenceOf' sxs txs
                                      | otherwise           = isSubsequenceOf' s txs


capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f $ words s
  where f w@(ws:wss) = (w, toUpper ws : wss)


capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (s:ss) = toUpper s : ss

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . correct . words
  where correct xs  = zipWith cap bs xs
          where bs  = take (length xs) [True] ++ map (elem '.') xs
                cap = (\b x -> bool x (capitalizeWord x) b)

        

data Button = B1    | B2 | B3
            | B4    | B5 | B6
            | B7    | B8 | B9
            | BStar | B0 | BHash
            deriving (Eq, Enum, Show)

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol lol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Haha thanks just making sure rofl ur turn"
        ]

buttons =  [B1, B1, B1, B4, B4, B4, B9, B9]

allkeys    = ["1", "abc2", "def3", "ghi4", "jkl5", "mno6", "pqrs7", "tuv8", "wxyz9", "*^", " 0+_", "#.,"]
allbuttons = enumFrom B1
kb         = zip allkeys allbuttons

toButtons :: [Char] -> [[Button]]
toButtons = concat . map toButton 

toButton :: Char -> [[Button]]
toButton c | isUpper c = [[BStar]] ++ (filter (/=[]) $ map fn kb)
           | otherwise = (filter (/=[]) $ map fn kb)
  where fn x = bool [] (replicate (1+index (elemIndex char (fst x))) $ snd x) (elem char (fst x))
          where index (Just x) = x
                char           = toLower c

toChars :: [[Button]] -> [Char]
toChars = foldr capitalize [] . map toChar 
  where capitalize x y | x == '*' && y /= []  = [toUpper $ head y] ++ tail y
                       | otherwise            = (x:y)

toChar :: [Button] -> Char
toChar bs = cvector !! ((len - 1) `mod` (length cvector))
  where cvector        = fst $ kb !! (index $ elemIndex (head bs) $ snd (unzip kb))
        index (Just x) = x
        len            = length bs


fingertaps = map (length . concat . toButtons) convo

popular :: (Eq a, Ord a) => [a] -> a
popular = head . maximumBy predicate . group . sort
  where predicate x y = bool GT LT (length x < length y)

mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy predicate . group . filter (/=' ') . sort
  where predicate x y = bool GT LT (length x < length y)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . map mostPopularLetter

coolestWord :: [String] -> String
coolestWord = popular . map coolestWord'
  where coolestWord' x = popular . words $  x


data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit l)     = l
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit l)     = show l
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
