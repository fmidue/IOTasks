{-# LANGUAGE TypeApplications #-}
module HangmanSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks
import Test.QuickCheck (arbitrary,elements,forAll,listOf1,maxSize,maxSuccess,quickCheckWith,stdArgs,suchThat)

-- hangman
hangmanSpec :: [Integer] -> Specification
hangmanSpec word = tillExit (
     branch (winCond $ allValues g) (writeOutput [text "correct!"] <> exit) mempty
  <> writeOutput [text "Game state:"  <> wildcard]
  <> readInput g digits AssumeValid
  <> branch ((currentValue g `isIn` listLit word) .&&. (currentValue g `isNotIn` valuesBefore 1 g))
    (writeOptionalOutput [text "good guess!"])
    (writeOptionalOutput [text "wrong guess!"])
  )
  where
    winCond :: Term k [Integer] -> Term k Bool
    winCond g = foldr (\a b -> intLit a `isIn` g .&&. b) true word
    g = intVar "g"

digits :: ValueSet Integer
digits = (singleton 0 `union` greaterThan 0) `intersection` lessThan 10

hangmanProg :: MonadTeletype m => [Integer] -> m ()
hangmanProg word = go [] where
  go guessed
    | Prelude.all (`Prelude.elem` guessed) word = putStrLn "correct!"
    | otherwise = do
        putStrLn $ "Game state:" ++ printWord word guessed
        -- putStrLn "guess a number!"
        x <- read <$> getLine
        if x `Prelude.elem` word Prelude.&& x `Prelude.notElem` guessed
          then do
            putStrLn "good guess!" -- this is optional
            go (x:guessed)
          else do
            putStrLn "wrong guess!" -- this is optional
            go guessed

printWord :: (Eq a, Show a) => [a] -> [a] -> String
printWord xs guessed = Prelude.foldr (\x -> (++) (if x `Prelude.elem` guessed then show x ++ " " else "_ ")) "" xs

spec :: Spec
spec =
  describe "taskCheck (hangmanProg w) (hangmanSpec w)" $
    it "succeeds for small unfolding parameter" $
      quickCheckWith Test.QuickCheck.stdArgs{maxSuccess = 5, maxSize = 4} $ forAll (listOf1 (elements [0..9])) $ \w ->
        taskCheckWith Test.IOTasks.stdArgs{maxIterationUnfold = 7} (hangmanProg w) (hangmanSpec w)
