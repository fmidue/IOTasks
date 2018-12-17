module Examples.Hangman where

import Prelude hiding (getLine, putStrLn, print)

import Types
import Interaction
import IOtt

import Data.List (nub,permutations,delete)

-- this Spec is slightly to loose
hangmanSpec :: [Int] -> Spec
hangmanSpec word =
  let letters = nub word
      perms = permutations letters
  in Choice $ permutationPath <$> perms

permutationPath :: [Int] -> Spec
permutationPath xs = StepSpecs $ go [] xs where
  --steps = foldl go _ xs
  go _ [] = [(Out (string "Correct", []),displayValue)]
  go is (y:ys) =
    (In ("xs", DListTy (SPred NumTy (predicate is y)) [("", exact y)]), prompt "which number?" <& mayReactWith (\(IntVal z) -> if z == y then Template (DontCare Nil) else MatchExactly "wrong!"))
    : go (y:is) ys
  predicate is y i = i `elem` is || i `notElem` delete y xs

-- TODO: improve Specs, so that the comments can be used for real
hangmanProg :: [Int] -> IOtt ()
hangmanProg word = go [] where
  go guessed
    | all (`elem` guessed) word = putStrLn "Correct"
    | otherwise = do
      --putStrLn ""
      --putStrLn $ printWord word guessed
      putStrLn "which number?"
      x <- read <$> getLine
      if x `elem` word && x `notElem` guessed
        then do
          putStrLn "good Guess!" -- this is optional
          go (x:guessed)
        else do
          putStrLn "wrong!"
          go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed
  = foldr (\x -> (++) (if x `elem` guessed then "x " else "_ ")) "" xs
