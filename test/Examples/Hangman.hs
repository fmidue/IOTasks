{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTasks
import Test.IOTasks.Term.ITerm
import Test.IOTasks.Term.ITerm.Prelude as T

-- Specification and corresponding implementation of
-- a simple hangman game.

-- possible elements to build the secret from
hangmanDomain :: [Int]
hangmanDomain = [0..9]

hangmanSpec :: [Int] -> Specification ITerm
hangmanSpec (lit -> word) =
  tillExit (
       branch (winCondition $ getAll "guessed")
         nop
         (writeFixedOutput [anything <> text "correct" <> anything] <> exit)
    <> writeFixedOutput [text "Game state:" <> anything]
    <> readInput "guessed" (intValues [0..9])
    <> branch ((\gs -> (T.last gs `T.elem` word) T.&& (T.last gs `T.notElem` T.init gs)) $ getAll @Int "guessed")
         (optional $ writeFixedOutput [text "wrong!"])
         (optional $ writeFixedOutput [text "good guess!"])
    )
  where
    winCondition :: ITerm [Int] -> ITerm Bool
    winCondition xs = T.all (`T.elem` T.filter (`T.elem` word) xs) word


hangmanProg :: MonadTeletype m => [Int] -> m ()
hangmanProg word = go [] where
  go guessed
    | Prelude.all (`Prelude.elem` guessed) word = putStrLn "correct"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        putStrLn "guess a number!"
        x <- read <$> getLine
        if x `Prelude.elem` word Prelude.&& x `Prelude.notElem` guessed
          then do
            putStrLn "good guess!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "wrong!" -- this is optional
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = Prelude.foldr (\x -> (++) (if x `Prelude.elem` guessed then show x ++ " " else "_ ")) "" xs
