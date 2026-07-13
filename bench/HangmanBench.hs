import Prelude hiding (putStrLn, print, getLine)
import Test.IOTasks hiding (hSetBuffering)
import Test.IOTasks.Z3 (satPaths)
import Test.IOTasks.Constraints (constraintTree)

import System.Clock (Clock(Monotonic), getTime, TimeSpec (TimeSpec))
import Control.Monad (forM_)
import System.IO (stdout, BufferMode (NoBuffering), hSetBuffering)

import qualified Data.List as List
import Data.Char (toUpper)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "running full test procedure"
  simpleTime "full test short" $ testHangman short 10
  simpleTime "full test medium" $ testHangman medium 10
  simpleTime "full test long" $ testHangman long 10

  putStrLn "searching for sat. paths"
  forM_ [0..4] $ \i -> do
    putStrLn $ unwords ["##",show (unfolds + i), "unfolds"]
    print =<< simpleTime "short word" (hangmanSatPaths short $ unfolds+i)
    print =<< simpleTime "medium word" (hangmanSatPaths medium $ unfolds+i)
    print =<< simpleTime "long word" (hangmanSatPaths long $ unfolds+i)
    putStrLn ""
  where
    unfolds = 8
    short = "TEST" -- 3 diff. letters
    medium = "WORDS" -- 5 diff. letters
    long = "PROGRAMS" -- 7 diff. letters

testHangman :: [Char] -> Int -> IO ()
testHangman w i = taskCheckWith Test.IOTasks.stdArgs{maxIterationUnfold = i, terminalOutput = False} (hangmanMain w) (hangmanSpec w)

hangmanSatPaths :: [Char] -> Int -> IO Int
hangmanSatPaths w i = (length . fst) <$> satPaths (0,i) to (constraintTree $ hangmanSpec w) maxSeqLength checkOverflows
  where
    to = solverTimeout stdArgs
    checkOverflows = avoidOverflows stdArgs
    maxSeqLength = solverMaxSeqLength stdArgs

simpleTime :: String -> IO a -> IO a
simpleTime label act = do
  start <- getTime Monotonic
  x <- act
  end <- getTime Monotonic
  let diff = end - start
  putStrLn $ unwords [label<>":","time:",show $ clockToSec diff, "sec"]
  pure x

clockToSec :: TimeSpec -> Double
clockToSec (TimeSpec s ns) = fromIntegral s + fromIntegral ns / (10^9)

-- string based hangman
hangmanSpec :: String -> Specification
hangmanSpec word =
  tillExit $
    branch (winCond $ allValues guess) (writeOutput [text "you win"] <> exit) nop
    <> readInput guess letters AssumeValid
    <> branch correctGuess
      (writeOutput [text "correct"])
      (writeOutput [text "wrong"])
  where
    guess :: Var String
    guess = stringVar "guess"
    letters :: ValueSet String
    letters = fromListStr $ map List.singleton ['A'..'Z']

    correctGuess :: Term k Bool
    correctGuess =
      currentValue guess `elem'` listLit letteredWord
      .&&. (currentValue guess `notElem'` valuesBefore 1 guess)

    letteredWord :: [String]
    letteredWord = map (List.singleton . toUpper) word

    winCond :: Term k [String] -> Term k Bool
    winCond g = and' $ map (\a -> strLit a `elem'` g) letteredWord

hangmanMain :: String -> IOrep ()
hangmanMain word = playRound []
  where
    playRound guesses =
      if isWin normalizedWord guesses
        then putStrLn "you win"
        else do
          guess <- readGuess
          if guess `elem` normalizedWord && guess `notElem` guesses
            then putStrLn "correct"
            else putStrLn "wrong"
          playRound (guess : guesses)

    normalizedWord = map toUpper word

isWin :: String -> [Char] -> Bool
isWin word guess = all (`elem` guess) word

readGuess :: IOrep Char
readGuess = do
  l <- getLine
  case l of
    [c] -> pure c
    _ -> readGuess
