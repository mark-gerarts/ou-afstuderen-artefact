{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hangman where

import Data.List (intersect, (\\))
import Data.Text (chunksOf, isInfixOf, singleton)
import Task (Task (..), select, view)
import Visualize (visualizeTask)
import "tophat" Prelude

data GameState = GameState
  { lives :: Int,
    wordToGuess :: Text,
    usedLetters :: [Text]
  }

-- The "random" word is hardcoded, because otherwise we'd need to add random to
-- our package.json. We don't want to add dependencies just for the examples.
main :: IO ()
main = visualizeTask <| hangman "ENDOFUNCTOR"

isLost :: GameState -> Bool
isLost = lives >> (== 0)

isWon :: GameState -> Bool
isWon GameState {..} =
  let wordLetters = chunksOf 1 wordToGuess
   in wordLetters `intersect` usedLetters == wordLetters

hangman :: Text -> Task h Text
hangman wordToGuess = gameStateToTask initialState
  where
    initialState =
      GameState
        { lives = 5,
          wordToGuess = wordToGuess,
          usedLetters = []
        }

gameStateToTask :: GameState -> Task h Text
gameStateToTask s@GameState {..}
  | isWon s = view <| "You won! The word was: " <> wordToGuess
  | isLost s = view <| "You lost! The word was: " <> wordToGuess
  | otherwise = renderState s

renderState :: GameState -> Task h Text
renderState s@GameState {..} = do
  (_, selectedLetter) <- (renderWord >< renderLives) >< renderInput
  gameStateToTask (chooseLetter s selectedLetter)
  where
    renderWord = view <| maskWord wordToGuess usedLetters

    renderLives
      | lives == 1 = view "1 guess left!"
      | otherwise = view <| display lives <> " guesses left."

    renderInput =
      select
        <| fromList [(letter, view letter) | letter <- alphabet \\ usedLetters]

maskWord :: Text -> [Text] -> Text
maskWord word revealedCharacters =
  chunksOf 1 word
    |> map maskChar
    |> unwords
  where
    maskChar :: Text -> Text
    maskChar c
      | c `elem` revealedCharacters = c
      | otherwise = "_"

alphabet :: [Text]
alphabet = ['A' .. 'Z'] |> map singleton

chooseLetter :: GameState -> Text -> GameState
chooseLetter s@GameState {..} letter =
  let newLives = if letter `isInfixOf` wordToGuess then lives else lives - 1
      newLetters = letter : usedLetters
   in s {lives = newLives, usedLetters = newLetters}
