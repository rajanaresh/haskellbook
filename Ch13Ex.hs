module Ch13Ex where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char ( isLetter
                 , toLower)

palindrome :: IO ()
palindrome = do
  line1 <- getLine
  check . map toLower . filter isLetter $ line1
  where check s | s == reverse s = putStrLn "It's a palindrome!" >> palindrome
                | otherwise      = putStrLn "Nope!" >> exitSuccess


type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age | name /= "" && age > 0 = Right $ Person name age
                  | name == ""            = Left NameEmpty
                  | not (age > 0)         = Left AgeTooLow
                  | otherwise             = Left $ PersonInvalidUnknown $ 
                                            "Name was: " ++ show name ++ 
                                            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = 
  putStr "Enter Name: " >>
  getLine >>= 
  (\name -> 
     putStr "Enter Age: " >>
     getLine >>=
     (\age -> check $ mkPerson name (read age::Integer)))
  where check p@(Right _) = putStr "Yay! Successfully got a person: " >> print p
        check (Left x)    = putStr "Errrrr: " >> print x
        
