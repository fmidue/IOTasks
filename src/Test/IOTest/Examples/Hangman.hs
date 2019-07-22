module Test.IOTest.Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOtt
import Test.IOTest.Language

-- hangmanSpec :: [Int] -> Specification
-- hangmanSpec word =
--   tillE (
--        branch (winCondition <$> getAllI "guessed")
--          nop
--          (writeOutputP [Contains (pure "Correct") "Correct"] <> e)
--     <> writeOutputP [Everything]
--     <> readInputI "guessed" NatTy
--     <> branch ((`elem` word) <$> getCurrentI "guessed")
--          (optional $ writeOutputP [Contains (pure "wrong") "wrong"])
--          (optional $ writeOutputP [Contains (pure "good Guess") "good Guess"])
--     )
--   where
--     winCondition xs = all (`elem` filter (`elem` word) xs) word
--
--
-- -- TODO: improve Specs, so that the comments can be used for real
-- -- mainly multiline "prompts" is what's missing
-- hangmanProg :: TeletypeM m => [Int] -> m ()
-- hangmanProg word = go [] where
--   go guessed
--     | all (`elem` guessed) word = putStrLn "Correct"
--     | otherwise = do
--         putStrLn $ "\n" ++ printWord word guessed ++ "\nwhich number?"
--         x <- read <$> getLine
--         if x `elem` word && x `notElem` guessed
--           then do
--             putStrLn "good Guess!" -- this is optional
--             go (x:guessed)
--           else do
--             putStrLn "wrong!"
--             go guessed
--
-- printWord :: (Eq a, Show a) => [a] -> [a] -> String
-- printWord xs guessed = foldr (\x -> (++) (if x `elem` guessed then show x ++ " " else "_ ")) "" xs
