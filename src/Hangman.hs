module Hangman where

import Puzzle
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]
import Control.Monad (forever)
import Data.Maybe
import Data.Char


newtype WordList =
  WordList [String]
  deriving (Eq, Show)


allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


minWordLength :: Int
minWordLength = 0


maxWordLength :: Int
maxWordLength = 45


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l >= minWordLength && l <= maxWordLength


cont :: Char -> String -> Bool
cont c w = (length $ filter(\x -> x == c) w) > 0


contS :: String -> String -> Bool
contS set w = foldl (\y x -> y && (cont x w)) True set


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 67 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle w filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn ("You win! Word is: " ++ w)
       exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> (handleGuess puzzle c >>= runGame)
    _ -> putStrLn "Your guess must be a single character"

