{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOrep
import Test.IOTest.Language
import Test.IOTest.Term as T

-- possible elements to build the secret from
hangmanDomain :: [Int]
hangmanDomain = [0..9]

hangmanSpec :: [Int] -> Specification
hangmanSpec (lit -> word) =
  tillExit (
       branch (winCondition $ T.getAll "guessed")
         nop
         (writeFixedOutput ["_correct_"] <> exit)
    <> writeFixedOutput ["Game state:_"]
    <> readInput "guessed" (intValues [0..9])
    <> branch ((\gs -> (T.last gs `T.elem` word) T.&& (T.last gs `T.notElem` T.init gs)) $ T.getAll @Int "guessed")
         (optional $ writeFixedOutput ["wrong!"])
         (optional $ writeFixedOutput ["good guess!"])
    )
  where
    winCondition :: Term [Int] -> Term Bool
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
