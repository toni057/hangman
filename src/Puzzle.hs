module Puzzle where

import Data.List

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (\x -> Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) ch = elem ch w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = elem ch guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just ch) = ch
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessedSoFar) c =
  Puzzle word newFilledInSoFar (c : guessedSoFar)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
        (_, True) -> do
          putStrLn "You already guessed that\
          \ character, pick \
          \ something else!"
          return puzzle
        (True, _) -> do
          putStrLn "This character was in the\
          \ word, filling in the word\
          \ accordingly"
          return (fillInCharacter puzzle guess)
        (False, _) -> do
          putStrLn "This character wasn't in\
          \ the word, try again."
          return (fillInCharacter puzzle guess)
