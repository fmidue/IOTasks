{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOtt
import Test.IOTest.Language

-- possible elements to build the secret from
hangmanDomain :: [Int]
hangmanDomain = [0..9]

hangmanSpec :: [Int] -> Specification
hangmanSpec word =
  tillE (
       branch (winCondition <$> getAll @Int "guessed")
         nop
         (writeFixedOutput ["_correct_"] <> e)
    <> writeFixedOutput ["Game state:_"]
    <> readInput "guessed" (intValues [0..9])
    <> branch ((\gs -> (last gs `elem` word) && last gs `notElem` init gs) <$> getAll @Int "guessed")
         (optional $ writeFixedOutput ["wrong!"])
         (optional $ writeFixedOutput ["good guess!"])
    )
  where
    winCondition xs = all (`elem` filter (`elem` word) xs) word


hangmanProg :: TeletypeM m => [Int] -> m ()
hangmanProg word = go [] where
  go guessed
    | all (`elem` guessed) word = putStrLn "correct"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        putStrLn "guess a number!"
        x <- read <$> getLine
        if x `elem` word && x `notElem` guessed
          then do
            putStrLn "good guess!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "wrong!" -- this is optional
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = foldr (\x -> (++) (if x `elem` guessed then show x ++ " " else "_ ")) "" xs
