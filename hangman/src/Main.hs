module Main where


import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Data.List ( take
                 , repeat
                 )
import Data.Bool (bool)

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9 

allWords :: IO WordList
allWords = do 
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = let l = length (w :: String)
                       in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] 

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) ch = elem ch word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = elem ch guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing  = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar incorrectList 
  where zipper guessed wordChar guessChar | wordChar == guessed = Just wordChar
                                          | otherwise           = guessChar
        incorrectList = bool (c:s) s (elem c word)
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) | (length guessed) > 7 = do
                                            putStrLn "You lose!"
                                            putStrLn $ "The word was: " ++ wordToGuess
                                            exitSuccess
                                        | otherwise            = return ()
gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) | all isJust filledInSoFar = do
                                       putStrLn $ "You win! The word was " ++ word
                                       exitSuccess
                                   | otherwise                = return ()

runGame :: Puzzle -> IO ()
runGame puzzle = do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
