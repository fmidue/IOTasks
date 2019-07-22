{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOtt
import Test.IOTest.Language

-- possible elements to build the secret from
hangmanDomain :: [Int]
hangmanDomain = [1..100]

hangmanSpec :: [Int] -> Specification
hangmanSpec word =
  tillE (
       branch (winCondition <$> getAll @Int "guessed")
         nop
         (writeFixedOutput ["_Correct_"] <> e)
    <> writeFixedOutput ["_"]
    <> readInput "guessed" (intValues hangmanDomain)
    <> branch ((`elem` word) <$> getCurrent @Int "guessed")
         (optional $ writeFixedOutput ["_wrong_"])
         (optional $ writeFixedOutput ["_good Guess_"])
    )
  where
    winCondition xs = all (`elem` filter (`elem` word) xs) word


hangmanProg :: TeletypeM m => [Int] -> m ()
hangmanProg word = go [] where
  go guessed
    | all (`elem` guessed) word = putStrLn "Correct"
    | otherwise = do
        putStrLn $ "\n" ++ printWord word guessed ++ "\nwhich number?"
        x <- read <$> getLine
        if x `elem` word && x `notElem` guessed
          then do
            putStrLn "good Guess!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "wrong!"
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = foldr (\x -> (++) (if x `elem` guessed then show x ++ " " else "_ ")) "" xs
