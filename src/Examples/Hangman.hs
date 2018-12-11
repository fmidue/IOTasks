module Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Types
import Interaction
import IOtt

import Data.List (nub,permutations)

hangmanSpec :: [Int] -> Spec
hangmanSpec word =
  let letters = nub word
      perms = permutations letters
  in Choice $ permutationPath <$> perms

permutationPath :: [Int] -> Spec
permutationPath xs = StepSpecs $ go xs ++ [(Out (line "Correct", []),displayValue)] where
  go = map (\x -> (In ("xs", DListTy IntTy [("", Exact x)]), doNothing))

-- TODO: improve Specs, so that the comments can be used for real
hangmanProg :: [Int] -> IOtt ()
hangmanProg word = go [] where
  go guessed
    | all (`elem` guessed) word = putStrLn "Correct"
    | otherwise = do
      --putStrLn ""
      --putStrLn $ printWord word guessed
      --putStrLn "\nwhich number?"
      x <- read <$> getLine
      if x `elem` word && x `notElem` guessed
        then go (x:guessed)
        else do
          --putStrLn "wrong!"
          go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed
  = foldr (\x -> (++) (if x `elem` guessed then "x " else "_ ")) "" xs
